{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Prelude hiding (Word, pi)

import Clash.Class.Resize (truncateB, zeroExtend)
import Clash.Class.BitPack (pack, unpack)
import Data.Bits
import Data.Char (ord, isSpace, isHexDigit, digitToInt, isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Printf
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hClose, IOMode(WriteMode), openFile)

import CFM.Types
import CFM.Inst
import Target
import Target.RTL

main :: IO ()
main = do
  [inputPath, outputPath] <- getArgs
  input <- readFile inputPath
  putStrLn "Bootstrapping..."
  (_, c) <- runIORTL $ do
    r <- bootstrap interpreter input
    case r of
      Left e -> liftIO $ do
        print e
        exitWith (ExitFailure 1)
      Right h -> do
        liftIO $ print (fromIntegral h :: Integer)
        out <- liftIO $ openFile outputPath WriteMode
        forM_ [0 .. h-1] $ \a -> do
          x <- tload a
          liftIO $ hPrintf out "%04x\n" (fromIntegral x :: Int)
        liftIO $ hClose out
  putStrLn $ "Cycles: " ++ show c

type Name = [Word]

nameFromString :: String -> Name

nameFromString [] = [0]
nameFromString (c : cs) = w0 : packS cs
  where
    c2w = fromIntegral . ord
    w0 = fromIntegral (length cs + 1) .|. (c2w c `shiftL` 8)
    packS (x : y : rest) = (c2w x .|. (c2w y `shiftL` 8)) : packS rest
    packS [x] = [c2w x]
    packS [] = []

-- | Converts a 'Word' into the 'WordAddr' it would dereference as, by dropping
-- the LSB.
word2wa :: Word -> WordAddr
word2wa x = truncateB (x `shiftR` 1)

wa2word :: WordAddr -> Word
wa2word x = zeroExtend x `shiftL` 1

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p s = case dropWhile p s of
  [] -> []
  (_ : rest) -> rest

ret, invert :: Inst
ret = NotLit $ ALU True T False False False (Res 0) (-1) 0
invert = NotLit $ ALU False NotT False False False (Res 0) 0 0

parseHex :: String -> Word
parseHex = foldl' (\n c -> n * 16 + fromIntegral (digitToInt c)) 0

newtype BsT m x = BsT { unBsT :: StateT FS (ExceptT ForthErr m) x }
  deriving (Functor, Applicative, Monad,
            MonadState FS, MonadError ForthErr, MonadIO)

instance MonadTrans BsT where
  lift = BsT . lift . lift

instance (MonadTarget m) => MonadTarget (BsT m) where
  tload = lift . tload
  tpush = lift . tpush
  tpushR = lift . tpushR
  tcall = lift . tcall

  tpop = lift tpop
  tpopR = lift tpopR

  tstore a = lift . tstore a

data ForthState = Interpreting | Compiling
     deriving (Eq, Show)

data FS = FS
  { fsInput :: String
  , fsCache :: M.Map KnownXT WordAddr
  , fsFallbacks :: M.Map String Int
  , fsParsers :: S.Set String
  , fsMasked :: S.Set String
  }

data KnownXT = CommaXT
             | CompileCommaXT
             | SfindXT
             | TickSourceXT
             | ToInXT
    deriving (Show, Eq, Enum, Bounded, Ord)

nameForXT :: KnownXT -> String
nameForXT x = case x of
  CommaXT         -> ","
  CompileCommaXT  -> "compile,"
  SfindXT         -> "sfind"
  TickSourceXT    -> "'SOURCE"
  ToInXT          -> ">IN"

data ForthErr = WordExpected
              | UnknownWord String
              | BadState ForthState ForthState String
              | ParserPhase
  deriving (Eq, Show)

hostlog :: (MonadIO m) => String -> BsT m ()
hostlog m = liftIO $ putStrLn m

bootstrap :: (MonadTarget m, MonadIO m)
          => BsT m () -> String -> m (Either ForthErr WordAddr)
bootstrap a source = do
  r <- runExceptT $ runStateT (unBsT (initializeTarget >> a)) s
  case r of
    Left e -> pure $ Left e
    Right (_, s') -> do
      liftIO $ putStrLn "Frequency of fallback emulation after last evolution:"
      forM_ (M.toList (fsFallbacks s')) $ \(w, c) ->
        liftIO $ putStrLn $ w ++ ": " ++ show c
      Right . word2wa <$> readHere
  where
    s = FS
      { fsInput = source
      , fsCache = M.empty
      , fsFallbacks = M.empty
      , fsParsers = S.empty
      , fsMasked = S.empty
      }

clearFallbacks :: Monad m => BsT m ()
clearFallbacks = modify $ \s -> s { fsFallbacks = M.empty }

-- | Sets up the basic memory contents we expect in the target system.
initializeTarget :: (MonadIO m, MonadTarget m) => BsT m ()
initializeTarget = do
  tstore 0 0  -- reset vector
  tstore 1 0  -- interrupt vector
  tstore 2 0  -- vocabulary root
  tstore 3 14 -- dictionary pointer
  tstore 4 0x1FF0 -- user area base
  tstore 5 0  -- STATE
  tstore 6 0  -- compilation freeze pointer
  hostlog "Target initialized."

cached_1_0 :: (MonadTarget m)
           => KnownXT
           -> Word
           -> BsT m ()
           -> BsT m ()
cached_1_0 name arg impl = do
  mxt <- gets $ M.lookup name . fsCache
  case mxt of
    Nothing -> impl
    Just xt -> tpush arg >> tcall xt

rescan :: (MonadIO m, MonadTarget m) => BsT m ()
rescan = do
  h <- readHere
  hostlog $ "Evolving target, HERE=" ++ show h
  forM_ [minBound .. maxBound] $ \xt -> do
    mx <- lookupWord $ nameForXT xt
    case mx of
      Nothing -> pure ()
      Just (cfa, _) -> do
        hostlog $ "Found " ++ nameForXT xt ++ " at " ++ show cfa
        modify $ \s -> s { fsCache = M.insert xt cfa (fsCache s) }

endOfInput :: (Monad m) => BsT m Bool
endOfInput = gets $ null . fsInput

takeWord :: (Monad m) => BsT m String
takeWord = do
  (w, rest) <- gets $ break isSpace . dropWhile isSpace . fsInput
  when (null w) (throwError WordExpected)
  modify $ \s -> s { fsInput = if null rest then [] else tail rest }
  pure w

-- | Reads the vocabulary head cell, giving the LFA of the latest definition.
readLatest :: (MonadTarget m) => m WordAddr
readLatest = word2wa <$> tload 2

writeLatest :: (MonadTarget m) => WordAddr -> m ()
writeLatest = tstore 2 . wa2word

-- | Reads the dictionary pointer cell, giving the next free cell after the end
-- of the dictionary.
readHere :: (MonadTarget m) => m Word
readHere = tload 3

-- | Writes the dictionary pointer cell.
writeHere :: (MonadTarget m) => Word -> m ()
writeHere = tstore 3

readState :: (MonadTarget m) => m ForthState
readState = do
  f <- tload 5
  if f /= 0
    then pure Compiling
    else pure Interpreting

writeState :: (MonadTarget m) => ForthState -> m ()
writeState s = tstore 5 $ case s of
  Interpreting -> 0
  Compiling -> -1

readFreeze :: (MonadTarget m) => m Word
readFreeze = tload 6

writeFreeze :: (MonadTarget m) => Word -> m ()
writeFreeze = tstore 6

freeze :: (MonadTarget m) => m ()
freeze = readHere >>= writeFreeze

-- | Searches for a word in the target dictionary. Returns its code field
-- address and flags word if found.
lookupWord :: (MonadTarget m) => String -> BsT m (Maybe (WordAddr, Word))
lookupWord name = do
  mxt <- gets $ M.lookup SfindXT . fsCache
  case mxt of
    Nothing -> lookupWordH name
    Just xt -> lookupWordT name xt

-- | Host-emulated version of lookupWord.
lookupWordH :: (MonadTarget m) => String -> m (Maybe (WordAddr, Word))
lookupWordH nameS = readLatest >>= lookupWordFrom
  where
    name = nameFromString nameS

    lookupWordFrom 0 = pure Nothing

    lookupWordFrom lfa = do
      match <- nameEqual name (lfa + 1)
      if match
        then do
          let nl = fromIntegral $ length name
          flags <- tload (lfa + 1 + nl)
          pure $ Just (lfa + 1 + nl + 1, flags)
        else tload lfa >>= lookupWordFrom . word2wa

    nameEqual [] _ = pure True
    nameEqual (w : ws) a = do
      w' <- tload a
      if w' == w
        then nameEqual ws (a + 1)
        else pure False

-- | Target-implemented version of lookupWord based on SFind.
lookupWordT :: (MonadTarget m) => String -> WordAddr -> m (Maybe (WordAddr, Word))
lookupWordT name xt = do
  h <- (80 +) . aligned <$> readHere
  zipWithM_ tstore [word2wa h ..] $ nameFromString name
  tpush (h + 1)
  tpush (fromIntegral (length name))
  tcall xt
  flag <- tpop
  if flag == 0
    then tpop >> tpop >> pure Nothing
    else do
      flags <- tpop
      newXt <- tpop
      pure $ Just (word2wa newXt, flags)

-- | Encloses a cell into the dictionary. The cell is treated as data and is
-- frozen from fusion.
comma :: (MonadTarget m) => Word -> BsT m ()
comma x = cached_1_0 CommaXT x $ do
  rawComma x
  freeze

-- | Encloses a cell into the dictionary without having an opinion on whether
-- it's data or code.
rawComma :: (MonadTarget m) => Word -> BsT m ()
rawComma x = do
  h <- readHere
  writeHere (h + 2)
  tstore (word2wa h) x

-- | Compiles instructions for materializing a literal value.
literal :: (MonadTarget m) => Word -> BsT m ()
literal w = do
  inst $ Lit $ truncateB $ if w >= 0x8000 then complement w else w
  when (w >= 0x8000) $ inst invert

compile :: (MonadTarget m) => Word -> BsT m ()
compile w = cached_1_0 CompileCommaXT w $ do
  -- Fetch the instruction on the far end of the call.
  dst <- unpack <$> tload (word2wa w)
  case dst of
    -- Returning ALU instruction?
    NotLit (ALU True t tn tr nm _ (-1) dadj) ->
      -- Inline it without the return.
      inst $ NotLit $ ALU False t tn tr nm (Res 0) 0 dadj
    -- Otherwise, compile the call as planned.
    _ -> inst $ NotLit $ Call $ truncateB $ word2wa w

-- | Compiles an instruction into the target's dictionary. This is the analog
-- of the word asm, .
--
-- This is where simple peephole optimizations can occur, subject to the freeze
-- line.
rawInst :: (MonadTarget m) => Word -> BsT m ()
rawInst i = do
  h <- readHere
  fp <- readFreeze

  if fp == h
    then rawComma i
    else do -- Previous instruction is not frozen, we can do stuff.
      pi <- tload $ word2wa (h - 2)
      case () of
        -- (ALU without return) - (return) fusion
        _ | (pi .&. 0x704C) == 0x6000 && i == 0x700C -> fuse (pi .|. 0x100C)
        -- (call) - (return) fusion
        _ | (pi .&. 0xE000) == 0x4000 && i == 0x700C -> fuse (pi .&. 0x1FFF)
        _ | (i .&. 0xF0FF) == 0x6003
            && ((i .&. 0xF00) - 0x200 < 0x400 || (i .&. 0xF00) == 0x700)
            && pi == 0x6180
            -> fuse i
        _ | (i .&. 0xF0FF) == 0x6003
            && ((i .&. 0xF00) - 0x200 < 0x400 || (i .&. 0xF00) == 0x700)
            && pi == 0x6181
            -> fuse (i - 3)
        _ -> rawComma i

fuse :: (MonadTarget m) => Word -> BsT m ()
fuse i = do
  h <- readHere
  writeHere (h - 2)
  rawInst i

inst :: (MonadTarget m) => Inst -> BsT m ()
inst = rawInst . pack

aligned :: Word -> Word
aligned x = x + (x .&. 1)

align :: (MonadTarget m) => BsT m ()
align = do
  h <- readHere
  if (h .&. 1) /= 0
    then writeHere (h + 1)
    else pure ()

createHeader :: (MonadTarget m) => Name -> Word -> BsT m ()
createHeader name flags = do
  align
  h <- readHere
  readLatest >>= comma . wa2word
  mapM_ comma name
  comma flags

  writeLatest (word2wa h)

interpretationOnly, compileOnly :: (MonadTarget m) => String -> BsT m ()
interpretationOnly name = do
  s <- readState
  when (s /= Interpreting) (throwError (BadState s Interpreting name))

compileOnly name = do
  s <- readState
  when (s /= Compiling) (throwError (BadState s Compiling name))

fallback :: (MonadIO m, MonadTarget m) => String -> BsT m ()
fallback ":" = do
  interpretationOnly ":"
  mcreate
  writeState Compiling

fallback ";" = do
  compileOnly ";"
  inst ret
  freeze
  writeState Interpreting

fallback "exit" = do
  compileOnly "exit"
  inst ret

fallback "constant" = do
  interpretationOnly "constant"

  docon <- lookupWord "(docon)"
  case docon of
    Nothing -> throwError $ UnknownWord "(docon)"
    Just (xt, _) -> do
      v <- tpop
      w <- nameFromString <$> takeWord
      createHeader w 0
      inst $ NotLit $ Call $ truncateB xt
      comma v

fallback "variable" = do
  interpretationOnly "variable"
  dovar <- lookupWord "(dovar)"
  case dovar of
    Nothing -> throwError $ UnknownWord "(dovar)"
    Just (xt, _) -> do
      mcreate
      inst $ NotLit $ Call $ truncateB xt
  comma 0

fallback "asm," = do
  interpretationOnly "asm,"
  v <- tpop
  rawInst v

fallback "[" = do
  compileOnly "["
  writeState Interpreting

fallback "]" = do
  interpretationOnly "]"
  writeState Compiling

fallback "(" = modify $ \s -> s { fsInput = dropWhile' (/= ')') (fsInput s) }
fallback "\\" = modify $ \s -> s { fsInput = dropWhile' (/= '\n') (fsInput s) }

fallback ".(" = do
  i <- gets fsInput
  hostlog $ takeWhile (/= ')') i
  modify $ \s -> s { fsInput = dropWhile' (/= ')') i }

fallback "'" = do
  interpretationOnly "'"
  w <- takeWord
  me <- lookupWord w
  case me of
    Just (cfa, _) -> tpush (wa2word cfa)
    Nothing -> throwError $ UnknownWord w

fallback "if" = do
  compileOnly "if"
  readHere >>= tpush
  freeze
  inst $ NotLit $ JumpZ 0  -- placeholder

fallback "else" = do
  compileOnly "else"
  ifA <- tpop
  readHere >>= tpush
  freeze
  inst $ NotLit $ Jump 0  -- placeholder
  h <- readHere
  i <- tload (word2wa ifA)
  tstore (word2wa ifA) $ i .|. (h `shiftR` 1)

fallback "then" = do
  compileOnly "then"
  h <- readHere
  freeze
  a <- tpop
  i <- tload (word2wa a)
  tstore (word2wa a) $ i .|. (h `shiftR` 1)

fallback "<TARGET-EVOLVE>" = do
  interpretationOnly "<TARGET-EVOLVE>"
  rescan
  clearFallbacks

fallback "host." = do
  interpretationOnly "host."
  v <- tpop
  hostlog $ show (fromIntegral v :: Integer)

fallback "TARGET-PARSER:" = do
  w <- takeWord
  liftIO $ putStrLn $ "TARGET-PARSER: " ++ w
  modify $ \s -> s { fsParsers = S.insert w (fsParsers s) }

fallback "TARGET-MASK:" = do
  w <- takeWord
  liftIO $ putStrLn $ "TARGET-MASK: " ++ w
  modify $ \s -> s { fsMasked = S.insert w (fsMasked s) }

fallback ('$' : hnum) | all isHexDigit hnum = do
  s <- readState
  case s of
    Interpreting -> tpush $ parseHex hnum
    Compiling -> literal $ parseHex hnum

fallback num | all isDigit num = do
  s <- readState
  case s of
    Interpreting -> tpush $ fromIntegral (read num :: Integer)
    Compiling -> literal $ fromIntegral (read num :: Integer)
  
fallback unk = throwError (UnknownWord unk)

-- | Host-implemented (CREATE) wrapper.
mcreate :: (MonadIO m, MonadTarget m) => BsT m ()
mcreate = do
  interpretationOnly "(CREATE)"
  w <- nameFromString <$> takeWord
  createHeader w 0

callParser :: (MonadIO m, MonadTarget m)
           => WordAddr
           -> BsT m ()
callParser xt = do
  tsourcem <- gets $ M.lookup TickSourceXT . fsCache
  toinm <- gets $ M.lookup ToInXT . fsCache
  case (,) <$> tsourcem <*> toinm of
    Nothing ->
      throwError ParserPhase
    Just (tsource, toin) -> do
      -- Parse a name from host input.
      w <- takeWord
      liftIO $ putStrLn $ "Calling target parsing word " ++ show xt ++ " with: " ++ w
      -- Copy it into a PAD-like transient region. We'll copy it as a counted
      -- string because I've already got code for that.
      buf <- (word2wa . (+ 80) . aligned) <$> readHere
      zipWithM_ tstore [buf ..] (nameFromString w)
      -- Designate its location in 'SOURCE.
      tstore (tsource + 1) (wa2word buf + 1)
      tstore (tsource + 2) (fromIntegral $ length w)
      -- zero >IN
      tstore (toin + 1) 0
      -- execute
      tcall xt

interpreter :: (MonadIO m, MonadTarget m) => BsT m ()
interpreter = do
  modify $ \s -> s { fsInput = dropWhile isSpace (fsInput s) }
  eoi <- endOfInput
  unless eoi $ do
    w <- takeWord
    me <- lookupWord w
    mask <- gets fsMasked
    case me of
      Just (cfa, flags) | not (S.member w mask) -> do
        s <- readState
        case s of
          Interpreting
            | flags == 0 -> do
              ps <- gets fsParsers
              if S.member w ps
                then callParser cfa
                else tcall cfa
            | otherwise -> throwError (BadState Interpreting Compiling w)
          Compiling    -> if flags == 0
                            then compile $ wa2word cfa
                            else tcall cfa
      _ -> do
        modify $ \s -> s { fsFallbacks =
          M.insert w (M.findWithDefault 0 w (fsFallbacks s) + 1) (fsFallbacks s) }
        fallback w
    interpreter
