{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Prelude hiding (Word)

import Clash.Class.Resize (truncateB, zeroExtend)
import Clash.Class.BitPack (pack)
import Data.Bits
import Data.Char (ord, isSpace, isHexDigit, digitToInt, isDigit)
import Data.List (foldl')
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
  runIORTL $ do
    r <- bootstrap interpreter input
    case r of
      Left e -> liftIO $ do
        print e
        exitWith (ExitFailure 1)
      Right h -> do
        liftIO $ print $ (fromIntegral h :: Integer)
        out <- liftIO $ openFile outputPath WriteMode
        forM_ [0 .. h-1] $ \a -> do
          x <- tload a
          liftIO $ hPrintf out "%04x\n" (fromIntegral x :: Int)
        liftIO $ hClose out

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
  , fsCommaXT :: Maybe WordAddr
  , fsCompileCommaXT :: Maybe WordAddr
  , fsSfindXT :: Maybe WordAddr
  }

data ForthErr = WordExpected
              | UnknownWord String
              | BadState ForthState ForthState String
  deriving (Eq, Show)

hostlog :: (MonadIO m) => String -> BsT m ()
hostlog m = liftIO $ putStrLn m

bootstrap :: (MonadTarget m, MonadIO m)
          => BsT m () -> String -> m (Either ForthErr WordAddr)
bootstrap a source = do
  r <- runExceptT $ evalStateT (unBsT (initializeTarget >> a)) s
  case r of
    Left e -> pure $ Left e
    Right _ -> Right . word2wa <$> readHere
  where
    s = FS
      { fsInput = source
      , fsCommaXT = Nothing
      , fsCompileCommaXT = Nothing
      , fsSfindXT = Nothing
      }

-- | Sets up the basic memory contents we expect in the target system.
initializeTarget :: (MonadIO m, MonadTarget m) => BsT m ()
initializeTarget = do
  tstore 0 0  -- reset vector
  tstore 1 0  -- interrupt vector
  tstore 2 0  -- vocabulary root
  tstore 3 12 -- dictionary pointer
  tstore 4 0x1FF0 -- user area base
  tstore 5 0  -- STATE
  hostlog "Target initialized."

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

-- | Searches for a word in the target dictionary. Returns its code field
-- address and flags word if found.
lookupWord :: (MonadTarget m) => String -> BsT m (Maybe (WordAddr, Word))
lookupWord name = do
  mxt <- gets fsSfindXT
  case mxt of
    Nothing -> lookupWordH name
    Just xt -> lookupWordT name xt

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

lookupWordT :: (MonadTarget m) => String -> WordAddr -> m (Maybe (WordAddr, Word))
lookupWordT name xt = do
  h <- readHere
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


-- | Compares a name to the sequence of words stored starting at an address.
nameEqual :: (MonadTarget m) => Name -> WordAddr -> m Bool
nameEqual [] _ = pure True
nameEqual (w : ws) a = do
  w' <- tload a
  if w' == w
    then nameEqual ws (a + 1)
    else pure False

cached_1_0 :: (MonadTarget m)
           => (FS -> Maybe WordAddr)
           -> Word
           -> BsT m ()
           -> BsT m ()
cached_1_0 getter arg impl = do
  mxt <- gets getter
  case mxt of
    Nothing -> impl
    Just xt -> tpush arg >> tcall xt

-- | Encloses a cell into the dictionary.
comma :: (MonadTarget m) => Word -> BsT m ()
comma x = cached_1_0 fsCommaXT x $ do
  h <- readHere
  writeHere (h + 2)
  tstore (word2wa h) x

literal :: (MonadTarget m) => Word -> BsT m ()
literal w = do
  comma $ 0x8000 .|. (if w >= 0x8000 then complement w else w)
  when (w >= 0x8000) $ inst invert

compile :: (MonadTarget m) => Word -> BsT m ()
compile w = cached_1_0 fsCompileCommaXT w $
  comma $ (0x4000 .|.) $ zeroExtend $ word2wa w

inst :: (MonadTarget m) => Inst -> BsT m ()
inst = comma . pack

createHeader :: (MonadTarget m) => Name -> Word -> BsT m ()
createHeader name flags = do
  h <- readHere
  readLatest >>= comma . wa2word
  mapM_ comma name
  comma flags

  writeLatest (word2wa h)

takeWord :: (Monad m) => BsT m String
takeWord = do
  (w, rest) <- gets $ break isSpace . dropWhile isSpace . fsInput
  when (null w) (throwError WordExpected)
  modify $ \s -> s { fsInput = if null rest then [] else tail rest }
  pure w

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
  w <- nameFromString <$> takeWord
  createHeader w 0
  writeState Compiling

fallback ";" = do
  compileOnly ";"
  inst ret
  writeState Interpreting

fallback "constant" = do
  interpretationOnly "constant"
  v <- tpop
  w <- nameFromString <$> takeWord
  createHeader w 0
  literal v
  inst ret

fallback "," = do
  interpretationOnly ","
  v <- tpop
  comma v

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

fallback "begin" = do
  compileOnly "begin"
  readHere >>= tpush

fallback "again" = do
  compileOnly "again"
  a <- tpop
  inst $ NotLit $ Jump $ truncateB $ word2wa a

fallback "until" = do
  compileOnly "until"
  a <- tpop
  inst $ NotLit $ JumpZ $ truncateB $ word2wa a

fallback "if" = do
  compileOnly "if"
  readHere >>= tpush
  inst $ NotLit $ JumpZ 0  -- placeholder

fallback "else" = do
  compileOnly "else"
  ifA <- tpop
  readHere >>= tpush
  inst $ NotLit $ Jump 0  -- placeholder
  h <- readHere
  i <- tload (word2wa ifA)
  tstore (word2wa ifA) $ i .|. (h `shiftR` 1)

fallback "then" = do
  compileOnly "then"
  h <- readHere
  a <- tpop
  i <- tload (word2wa a)
  tstore (word2wa a) $ i .|. (h `shiftR` 1)

fallback "immediate" = do
  interpretationOnly "immediate"
  lfa <- readLatest
  nl <- (.&. 0xFF) <$> tload (lfa + 1)
  let nlWords = (nl `div` 2) + 1
  tstore (lfa + 1 + truncateB nlWords) 0xFFFF

fallback "<TARGET-EVOLVE>" = do
  interpretationOnly "<TARGET-EVOLVE>"
  rescan

fallback "host." = do
  interpretationOnly "host."
  v <- tpop
  hostlog $ show (fromIntegral v :: Integer)

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

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p s = case dropWhile p s of
  [] -> []
  (_ : rest) -> rest

endOfInput :: (Monad m) => BsT m Bool
endOfInput = gets $ null . fsInput

rescan :: (MonadIO m, MonadTarget m) => BsT m ()
rescan = do
  h <- readHere
  hostlog $ "Evolving target, HERE=" ++ show h
  do
    mx <- lookupWord ","
    case mx of
      Nothing -> pure ()
      Just (cfa, _) -> do
        hostlog $ "Found , at " ++ show cfa
        modify $ \s -> s { fsCommaXT = Just cfa }
  do
    mx <- lookupWord "compile,"
    case mx of
      Nothing -> pure ()
      Just (cfa, _) -> do
        hostlog $ "Found compile, at " ++ show cfa
        modify $ \s -> s { fsCompileCommaXT = Just cfa }
  do
    mx <- lookupWord "sfind"
    case mx of
      Nothing -> pure ()
      Just (cfa, _) -> do
        hostlog $ "Found sfind at " ++ show cfa
        modify $ \s -> s { fsSfindXT = Just cfa }

interpreter :: (MonadIO m, MonadTarget m) => BsT m ()
interpreter = do
  modify $ \s -> s { fsInput = dropWhile isSpace (fsInput s) }
  eoi <- endOfInput
  unless eoi $ do
    w <- takeWord
    me <- lookupWord w
    case me of
      Just (cfa, flags) -> do
        s <- readState
        case s of
          Interpreting -> if flags == 0
                            then tcall cfa
                            else throwError (BadState Interpreting Compiling w)
          Compiling    -> if flags == 0
                            then compile $ wa2word cfa
                            else tcall cfa
      Nothing -> fallback w
    interpreter

ret, invert :: Inst
ret = NotLit $ ALU True T False False False (Res 0) (-1) 0
invert = NotLit $ ALU False NotT False False False (Res 0) 0 0

parseHex :: String -> Word
parseHex = foldl' (\n c -> n * 16 + fromIntegral (digitToInt c)) 0
