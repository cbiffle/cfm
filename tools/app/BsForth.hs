{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Prelude hiding (pi)

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hClose, IOMode(WriteMode), openFile)
import Text.Printf
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Clash.Class.Resize (truncateB)
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Word
import CFM.Types
import Target
import Target.Emu

{-

ABI notes:

The shape of the system variables has changed. It's given by the SysVar enum.

The bootstrap now assumes the existence of user variables, and interacts with
the ones listed in UVar.

-}

------------------------------------------------------------------------------
-- Our Monad.

newtype ForthT m x = ForthT { runForthT :: ReaderT CFG
                                           (StateT FS
                                           (ExceptT ForthErr m)) x }
  deriving (Functor, Applicative, Monad, MonadError ForthErr, MonadIO,
            MonadReader CFG)

data CFG = CFG
  { cfgList :: Bool
  } deriving (Eq, Show)

data FS = FS
  { fsInput :: [String]
  , fsCatch :: Maybe Cell
  , fsAsm :: Maybe Cell
  } deriving (Eq, Show)

instance MonadTrans ForthT where
  lift = ForthT . lift . lift . lift

data ForthErr = UnknownWord String
              | PhaseError String
              | TargetError Cell
  deriving (Eq, Show)

takeInputLine :: (Monad m) => ForthT m (Maybe String)
takeInputLine = do
  i <- ForthT $ gets fsInput
  case i of
    (l : ls) -> do
      ForthT $ modify $ \s -> s { fsInput = ls }
      if l == "---"
        then takeInputLine
        else pure $ Just l
    [] -> pure Nothing

------------------------------------------------------------------------------
-- Wrapped target operations.
pop :: MonadTarget m => ForthT m Cell
pop = lift tpop >>= either (throwError . TargetError) pure

push :: MonadTarget m => Cell -> ForthT m ()
push x = lift $ tpush x

peek :: MonadTarget m => Cell -> ForthT m Cell
peek a = do
  x <- lift (tload $ truncateB $ a `shiftR` 1)
  either (throwError . TargetError) pure x

poke :: MonadTarget m => Cell -> Cell -> ForthT m ()
poke a = lift . tstore (truncateB $ a `shiftR` 1)

peek8 :: MonadTarget m => Cell -> ForthT m Word8
peek8 a = do
  x <- peek a
  pure $ fromIntegral $ if a .&. 1 /= 0
                           then x `shiftR` 8
                           else x

poke8 :: MonadTarget m => Cell -> Word8 -> ForthT m ()
poke8 a v = do
  x <- peek a
  poke a $ if a .&. 1 /= 0
             then (x .&. 0x00FF) .|. (fromIntegral v `shiftL` 8)
             else (x .&. 0xFF00) .|. fromIntegral v

data TString = TString Cell Cell
  deriving (Show)

pokeString :: MonadTarget m => Cell -> String -> ForthT m TString
pokeString a s = do
  zipWithM_ poke8 [a..] $ map (fromIntegral . fromEnum) s
  pure $ TString a $ fromIntegral $ length s

peekString :: MonadTarget m => TString -> ForthT m String
peekString (TString s n) = mapM (\a -> toEnum . fromIntegral <$> peek8 a)
                                [s..(s+n-1)]

------------------------------------------------------------------------------
-- Abstract variables.

class Var v where
  getv :: (MonadTarget m) => v -> ForthT m Cell
  setv :: (MonadTarget m) => v -> Cell -> ForthT m ()

------------------------------------------------------------------------------
-- System variable access.

data SysVar = ResetVector   -- ^ Starting address.
            | IrqVector     -- ^ Interrupt handler.
            | U0            -- ^ User area base.
            | RootWordlist  -- ^ Wordlist pointer for core definitions.
            | DP            -- ^ Dictionary pointer.
            | FREEZEP       -- ^ Frozen dictionary pointer.
            | VocLink       -- ^ Root of list of all vocabularies.
            deriving (Eq, Show, Enum, Bounded)

sysaddr :: SysVar -> Cell
sysaddr = fromIntegral . (* 2) . fromEnum

instance Var SysVar where
  getv v = peek $ sysaddr v

  setv v = poke (sysaddr v)

------------------------------------------------------------------------------
-- User variable access.

data UVar = HANDLER
          | STATE
          | SOURCE0
          | SOURCE1
          | ToIN
          | BASE
          | CURRENT
          | CONTEXT
          deriving (Eq, Show, Enum, Bounded)

instance Var UVar where
  getv v = do
    u <- getv U0
    peek (fromIntegral (fromEnum v * 2) + u)

  setv v x = do
    u <- getv U0
    poke (fromIntegral (fromEnum v * 2) + u) x

getSOURCE :: MonadTarget m => ForthT m TString
getSOURCE = TString <$> getv SOURCE0 <*> getv SOURCE1

setSOURCE :: MonadTarget m => TString -> ForthT m ()
setSOURCE (TString s e) = setv SOURCE0 s >> setv SOURCE1 e

------------------------------------------------------------------------------
-- Initial image prep.

initialUser :: Cell
initialUser = 0x7B80    -- TODO: base on RAM size

inputBuffer :: Cell
inputBuffer = initialUser - 80

nameBuffer :: Cell
nameBuffer = inputBuffer - 80

initializeVars :: MonadTarget m => ForthT m ()
initializeVars = do
  let numSysVars = fromEnum (maxBound @SysVar) + 1
      initialHERE = fromIntegral $ numSysVars * 2
  mapM_ (uncurry setv) [ (ResetVector,  0)
                       , (IrqVector,    2)
                       , (U0,           initialUser)
                       , (RootWordlist, 0)
                       , (DP,      initialHERE)
                       , (FREEZEP, initialHERE)
                       , (VocLink, 0)
                       ]
  mapM_ (uncurry setv) [ (HANDLER, 0)
                       , (STATE, 0)
                       , (SOURCE0, 0)
                       , (SOURCE1, 0)
                       , (ToIN, 0)
                       , (BASE, 10)
                       , (CURRENT, sysaddr RootWordlist)
                       , (CONTEXT, sysaddr RootWordlist)
                       ]

----------------
-- Hosted wordlists.

tick :: (MonadTarget m) => String -> ForthT m Cell
tick n = do
  ns <- pokeString nameBuffer n
  docon <- find ns
  case docon of
    Nothing -> throwError $ PhaseError $ n ++ " must be defined already"
    Just (xt, _) -> pure xt

outside :: (MonadIO m, MonadTarget m) => [(String, ForthT m ())]
outside =
  [ ("(", emuParen)
  , ("\\", emuWhack)
  , (".(", emuDotParen)
  , (":", emuColon)
  , ("asm,", emuAsmComma)
  , ("]", emuRBrack)
  , ("constant", emuConstant)
  , ("variable", emuVariable)
  , ("host.", emuHostDot)

  , ("<TARGET-CATCH>", targetCatch)
  , ("<TARGET-ASM>", targetAsm)
  ]

targetCatch :: (MonadTarget m) => ForthT m ()
targetCatch = do
  cxt <- tick "catch"
  ForthT $ modify $ \s -> s { fsCatch = Just cxt }

targetAsm :: (MonadTarget m) => ForthT m ()
targetAsm = do
  cxt <- tick "asm,"
  ForthT $ modify $ \s -> s { fsAsm = Just cxt }

emuParen :: (MonadTarget m) => ForthT m ()
emuParen = do
  TString s n <- getSOURCE
  tin <- getv ToIN
  TString s' n' <- skipWhileT (/= fromIntegral (fromEnum ')')) $
                   TString (s+tin) (n-tin)
  setv ToIN $ (s' + min n' 1) - s

emuDotParen :: (MonadIO m, MonadTarget m) => ForthT m ()
emuDotParen = do
  TString s n <- getSOURCE
  tin <- getv ToIN
  TString s' n' <- skipWhileT (/= fromIntegral (fromEnum ')')) $
                   TString (s+tin) (n-tin)
  setv ToIN $ (s' + min n' 1) - s
  text <- peekString $ TString (s+tin) (s' - (s+tin))
  liftIO $ putStrLn text

emuHostDot :: (MonadIO m, MonadTarget m) => ForthT m ()
emuHostDot = do
  x <- pop
  liftIO $ print (fromIntegral x :: Int)

emuWhack :: (MonadTarget m) => ForthT m ()
emuWhack = do
  TString _ n <- getSOURCE
  setv ToIN n

emuColon :: (MonadTarget m) => ForthT m ()
emuColon = do
  createCommon
  setv STATE 1

emuConstant :: (MonadTarget m) => ForthT m ()
emuConstant = do
  docon <- tick "(docon)"
  createCommon
  compileComma docon
  pop >>= comma

emuVariable :: (MonadTarget m) => ForthT m ()
emuVariable = do
  dovar <- tick "(dovar)"
  createCommon
  compileComma dovar
  comma 0

createCommon :: (MonadTarget m) => ForthT m ()
createCommon = do
  align
  h <- here
  wl <- getv CURRENT
  peek wl >>= comma
  poke wl h
  parseName >>= sComma
  comma 0  -- flags

inside :: (MonadIO m, MonadTarget m) => [(String, ForthT m ())]
inside =
  [ ("[", emuLBrack)
  , (";", emuSemi)
  , ("exit", emuExit)
  , ("(", emuParen)
  , ("\\", emuWhack)
  , (".(", emuDotParen)
  , ("if", emuIf)
  , ("else", emuElse)
  , ("then", emuThen)
  , ("postpone", emuPostpone)
  ]

emuLBrack :: (MonadTarget m) => ForthT m ()
emuLBrack = setv STATE 0

emuRBrack :: (MonadTarget m) => ForthT m ()
emuRBrack = setv STATE 1

emuAsmComma :: (MonadTarget m) => ForthT m ()
emuAsmComma = do
  x <- lift tpop
  either (throwError . TargetError) asmComma x

emuExit :: (MonadTarget m) => ForthT m ()
emuExit = asmComma 0x700C

emuSemi :: (MonadTarget m) => ForthT m ()
emuSemi = do
  emuExit
  emuLBrack

emuIf, emuElse, emuThen :: (MonadTarget m) => ForthT m ()
emuIf = markF 0x2000
emuThen = pop >>= resolveF
emuElse = do
  x <- pop
  markF 0
  resolveF x

emuPostpone :: (MonadTarget m) => ForthT m ()
emuPostpone = do
  n <- parseName
  mdef <- find n
  case mdef of
    Nothing -> peekString n >>= throwError . UnknownWord
    Just (xt, 0) -> do
      ccxt <- tick "compile,"
      literal xt
      compileComma ccxt
    Just (xt, _) -> compileComma xt

markF :: (MonadTarget m) => Cell -> ForthT m ()
markF x = do
  freeze >>= push
  asmComma x

resolveF :: (MonadTarget m) => Cell -> ForthT m ()
resolveF x = do
  inst <- peek x
  h <- freeze
  poke x $ inst .|. (h `shiftR` 1)

----------------
-- Emulated text interpreter.

skipWhileT :: (MonadTarget m) => (Word8 -> Bool) -> TString -> ForthT m TString
skipWhileT _ s@(TString _ 0) = pure s
skipWhileT f x@(TString s n) = do
  c <- peek8 s
  if f c
    then skipWhileT f (TString (s + 1) (n - 1))
    else pure x

quitloop :: (MonadIO m, MonadTarget m) => ForthT m ()
quitloop = do
  line <- takeInputLine
  case line of
    Nothing -> pure ()
    Just x -> do
      listing <- asks cfgList
      when listing $ liftIO $ putStrLn x
      ts <- pokeString inputBuffer x
      setSOURCE ts
      setv ToIN 0
      interpret
      quitloop

interpret :: (MonadIO m, MonadTarget m) => ForthT m ()
interpret = do
  n <- parseName
  case n of
    TString _ 0 -> pure ()
    _ -> do
      mdef <- find n
      s <- getv STATE
      case mdef of
        -- Normal definition in target:
        Just (xt, 0) ->
          if s /= 0
            then compileComma xt
            else execute xt
        -- Immediate definition in target:
        Just (xt, _) -> execute xt
        -- Definition does not exist in target:
        Nothing -> do
          nameStr <- peekString n
          fromMaybe (tryNumber nameStr) $ if s /= 0
            then lookup nameStr inside
            else lookup nameStr outside
      interpret

tryNumber :: (MonadTarget m) => String -> ForthT m ()
tryNumber s = do
  n <- tryNumber' s
  st <- getv STATE
  if st /= 0
    then literal n
    else lift $ tpush n

tryNumber' :: (MonadTarget m) => String -> ForthT m Cell
tryNumber' ['\'', c, '\''] = pure $ fromIntegral $ fromEnum c
tryNumber' ('$' : s) = do
  oldBase <- getv BASE
  setv BASE 16
  n <- tryNumber' s
        `catchError` (\e -> setv BASE oldBase >> throwError e)
  setv BASE oldBase
  pure n
tryNumber' ('-' : s) = negate <$> tryNumber' s

tryNumber' s = do
  b <- getv BASE
  when (any (not . digitInBase b) s) (throwError $ UnknownWord s)
  pure $ foldl' (\n c -> n * b + fromIntegral (digitToInt c)) 0 s
  where
    digitInBase b c
      | isDigit c = (fromEnum c - fromEnum '0') < fromIntegral b
      | c >= 'a' = (fromEnum c - fromEnum 'a') + 10 < fromIntegral b
      | c >= 'A' = (fromEnum c - fromEnum 'A') + 10 < fromIntegral b
      | otherwise = False

execute :: (MonadTarget m) => Cell -> ForthT m ()
execute xt = do
  catchXT <- ForthT $ gets fsCatch
  case catchXT of
    Just c -> do
      push xt
      lift $ tcall $ truncateB $ c `shiftR` 1
      r <- pop
      when (r /= 0) $ throwError $ TargetError r

    Nothing -> do
      lift $ tcall $ truncateB $ xt `shiftR` 1
      void $ peek 0  -- detect exception

find :: (MonadTarget m) => TString -> ForthT m (Maybe (Cell, Cell))
find n = do
  d <- findIn n =<< getv CONTEXT
  case d of
    Just _ -> pure d
    Nothing -> findIn n =<< getv CURRENT

findIn :: (MonadTarget m)
       => TString -> Cell -> ForthT m (Maybe (Cell, Cell))
findIn _ 0 = pure Nothing
findIn ts wl = do
  lfa <- peek wl
  let nfa = lfa + 2
  nlen <- fromIntegral <$> peek8 (lfa + 2)
  eq <- stringComp ts $ TString (nfa + 1) nlen
  if eq
    then do
      let ffa = aligned (nfa + 1 + nlen)
      flags <- peek ffa
      pure $ Just (ffa + 2, flags)
    else findIn ts lfa

stringComp :: (MonadTarget m) => TString -> TString -> ForthT m Bool
stringComp s1@(TString _ n1) s2@(TString _ n2)
  | n1 /= n2 = pure False
  | otherwise = do
  s1' <- peekString s1
  s2' <- peekString s2
  pure $ s1' == s2'

compileComma, asmComma, asmCommaE, rawComma, comma
  :: (MonadTarget m) => Cell -> ForthT m ()
compileComma xt = do
  i <- peek xt
  asmComma $ if (i .&. 0xF04C) == 0x700C
    then i .&. 0xEFF3
    else 0x4000 .|. (xt `shiftR` 1)

asmComma x = do
  maxt <- ForthT $ gets fsAsm
  case maxt of
    Nothing -> asmCommaE x
    Just axt -> do
      push x
      execute axt

asmCommaE ni = do
  h <- here
  fp <- getv FREEZEP

  if fp == h
    then rawComma ni
    else do
      pi <- peek (h - 2)

      case () of
        _ | (pi .&. 0xF04C) == 0x6000 && ni == 0x700C -> do
          allot (-2)
          asmComma $ 0x100C .|. pi
        _ | (pi .&. 0xE000) == 0x4000 && ni == 0x700C -> do
          allot (-2)
          asmComma $ pi .&. 0x1FFF
        _ | ((ni .&. 0xF0FF) == 0x6003 || (ni .&. 0xF0FF) == 0x6000)
            && ((ni .&. 0xF00) - 0x200 < 0x400 || (ni .&. 0xF00) == 0x700)
            && (pi .&. 0xFFFE) == 0x6180 -> do
          let i' = (ni + (pi .&. 1)) .&. 0xFFF3
              i'' = if (i' .&. 3) == 1 then i' .|. 0x80 else i'
          allot (-2)
          asmComma i''
        _ | pi == 0x6081  -- dup
            && (ni .&. 0xF0FF) == 0x6000  -- ALU w/o stack effect
            && (ni .&. 0x0F00) `elem` [0, 0x0600, 0x0C00]  -- unary op
            -> allot (-2) >> asmComma (ni .|. 0x81)
        _ -> rawComma ni

rawComma c = do
  h <- here
  poke h c
  allot 2

comma c = do
  rawComma c
  void freeze

freeze :: (MonadTarget m) => ForthT m Cell
freeze = do
  h <- here
  setv FREEZEP h
  pure h

here :: (MonadTarget m) => ForthT m Cell
here = getv DP

allot :: (MonadTarget m) => Cell -> ForthT m ()
allot u = do
  h <- here
  setv DP (h + u)

aligned :: Cell -> Cell
aligned x = x + (x .&. 1)

align :: (MonadTarget m) => ForthT m ()
align = do
  h <- here
  setv DP $ aligned h

literal :: (MonadTarget m) => Cell -> ForthT m ()
literal v = do
  let large = v .&. 0x8000 /= 0
  asmComma $ 0x8000 .|. (if large then complement v else v)
  when large $ asmComma invertInst

sComma :: (MonadTarget m) => TString -> ForthT m ()
sComma ts@(TString _ n) = do
  h <- here
  poke8 h (fromIntegral n)
  s <- peekString ts
  void $ pokeString (h+1) s
  setv DP $ h + 1 + n
  align

invertInst :: Cell
invertInst = 0x6600

-- | Scans a whitespace-delimited name from the unused portion of the input.
-- If end-of-input is reached, the result will be zero-length.
parseName :: (MonadTarget m) => ForthT m TString
parseName = do
  TString srcS srcL <- getSOURCE
  tin <- getv ToIN
  sw@(TString wordS _) <- skipWhileT (< 0x21) $
                          TString (srcS + tin) (srcL - tin)
  TString restS restL' <- skipWhileT (0x20 <) sw
  setv ToIN $ (restS + min 1 restL') - srcS
  pure $ TString wordS (restS - wordS)


----------------
-- Main.

bootstrap :: (MonadTarget m, MonadIO m)
          => ForthT m r -> [String] -> m (Either ForthErr r)
bootstrap a source = do
  let s = FS
        { fsInput = source
        , fsCatch = Nothing
        , fsAsm = Nothing
        }
      cfg = CFG { cfgList = False }
  runExceptT $ evalStateT (runReaderT (runForthT (initializeVars >> a)) cfg)
                          s

main :: IO ()
main = do
  [inputPath, outputPath] <- getArgs
  input <- lines <$> readFile inputPath
  putStrLn "Bootstrapping..."
  c <- runEmu $ do
    r <- bootstrap (quitloop >> here) input
    case r of
      Left e -> do
        liftIO $ print e
        liftIO $ exitWith $ ExitFailure 1
      Right h -> do
        liftIO $ print (fromIntegral h :: Int)
        out <- liftIO $ openFile outputPath WriteMode
        forM_ [0, 2 .. h-2] $ \a -> do
          Right x <- tload $ truncateB $ a `shiftR` 1
          liftIO $ hPrintf out "%04x\n" (fromIntegral x :: Int)
        liftIO $ hClose out
    cycles
  putStrLn $ "Cycles: " ++ show c
