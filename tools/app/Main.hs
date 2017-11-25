{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Text.Parsec hiding (spaces, State)
import Text.Parsec.String
import Text.Printf

import Data.Bits
import Data.Default
import Data.Char (isSpace)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word16)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO (openFile, IOMode(WriteMode), hClose)

import Control.Monad.State
import Control.Monad.Except

data AsmTop = Colon String [AsmFrag]
            | Constant String
            | Variable String
            | OrgDirective
            | ALUPrim String
            | Interp AsmFrag
            deriving (Show)

data AsmFrag = Word String
             | Comment String
             | Begin [AsmFrag] LoopEnd
             deriving (Show)

data LoopEnd = Again
             deriving (Show)

sourceFile = skipMany space >> many top <* eof

top = label colon "colon definition"
  <|> label constant "constant definition"
  <|> label variable "variable definition"
  <|> label org "org directive"
  <|> label aluprim "ALU primitive definition"
  <|> label interp "interpreted code"

colon = do
  sic ":" 
  Colon <$> lexeme <*> manyTill frag (sic ";")

constant = do
  sic "constant"
  Constant <$> lexeme

variable = do
  sic "variable"
  Variable <$> lexeme

org = sic "org" >> pure OrgDirective

aluprim = sic "alu:" >> ALUPrim <$> lexeme

interp = Interp <$> frag

frag = comment <|> loop <|> word

comment = Comment <$> (do sic "\\"
                          c <- many (noneOf "\n")
                          (newline *> maybeWs)
                          pure c
                       <|> sic "(" *> many (noneOf ")") <* char ')' <* maybeWs)
                  <?> "comment"

loop = uncurry Begin <$> (sic "begin" *> frag `manyTill'` loopEnd)
       <?> "loop"

loopEnd = sic "again" >> pure Again
                     

word = do
  w <- lexeme
  if w == ";"
    then unexpected "semicolon"
    else pure (Word w)

sic :: String -> Parser ()
sic s = try (string s *> ws)

lexeme :: Parser String
lexeme = many1 (satisfy (not . isSpace)) <* ws <?> "word"

ws :: Parser ()
ws = (skipMany1 space *> pure ()) <|> eof

maybeWs :: Parser ()
maybeWs = (skipMany space *> pure ()) <|> eof

manyTill' :: Stream s m t
          => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill' p end = scan
  where
    scan = (([],) <$> end)
        <|> ((\x (xs, e) -> (x : xs, e)) <$> p <*> scan)

---------

data Def = Compiled Int
         | InlineLit Int
         | RawInst Int
         | Immediate (Asm ())

instance Show Def where
  show (Compiled x) = "Compiled " ++ show x
  show (InlineLit x) = "InlineLit " ++ show x
  show (Immediate _) = "Immediate ..."

data AS = AS
  { asDict :: M.Map String Def
  , asStack :: [Int]
  , asPos :: Int
  , asMem :: M.Map Int Int
  } deriving (Show)

instance Default AS where
  def = AS M.empty [] 0 M.empty

newtype Asm a = Asm { runAsm :: ExceptT String (State AS) a }
  deriving (Functor, Applicative, Monad, MonadState AS, MonadError String)

push x = modify $ \s -> s { asStack = x : asStack s }
pop = do
  ss <- gets asStack
  case ss of
    [] -> throwError "stack underflow in interpreted code"
    (x : xs) -> do
      modify $ \s -> s { asStack = xs }
      pure x

here :: Asm Int
here = gets asPos

create n d = modify $ \s -> s { asDict = M.insert n d (asDict s) }

find n = do
  md <- gets (M.lookup n . asDict)
  case md of
    Nothing -> case reads n of
      [(x, "")] -> pure (InlineLit x)
      _         -> throwError ("use of unknown word " ++ n)
    Just d -> pure d

store v a = do
  dupe <- gets $ M.member a . asMem
  if dupe
    then throwError ("Duplicate definition at address " ++ show a)
    else modify $ \s -> s { asMem = M.insert a v (asMem s) }

comma v | 0 <= v && v < 65536 = do
  here >>= store v
  modify $ \s -> s { asPos = asPos s + 1 }
comma _ = error "internal error: value passed to comma out of range"
  
run (Colon name body) = do
  p <- here
  create name $ Compiled p
  mapM_ compile' body

run (Constant name) = do
  v <- pop
  create name $ InlineLit v

run (Variable name) = do
  p <- here
  create name $ InlineLit p
  comma 0

run OrgDirective = do
  a <- pop
  if a .&. 1 /= 0
    then throwError ("odd address in org directive: " ++ show a)
    else modify $ \s -> s { asPos = a `shiftR` 1 }

run (ALUPrim name) = do
  bits <- pop
  if 0 <= bits && bits < 65536
    then create name $ RawInst bits
    else throwError "ALU primitive must fit in 16 bits"

run (Interp (Comment _)) = pure ()

run (Interp (Word s)) = do
  d <- find s
  case d of
    Immediate f -> f
    InlineLit x -> push x
    _ -> throwError ("word " ++ s ++ " cannot be used in interpreted code")

run (Interp (Begin _ _)) = throwError "loops not permitted in interpreted code"

compile' (Comment _) = pure ()
compile' (Word w) = find w >>= compile
compile' (Begin body Again) = do
  begin <- here
  mapM_ compile' body
  jmp begin

compile (Compiled a)
  | a < 8192 = comma (0x4000 .|. a)
  | otherwise = error "internal error: colon def out of range"

compile (InlineLit x)
  | 0 <= x && x < 32768 = comma (x .|. 0x8000)
  | -32768 <= x && x < 0 = comma (complement x .|. 0x8000)
  | otherwise = error "internal error: literal out of range"

compile (Immediate f) = f

compile (RawInst i)
  | 0 <= i && i < 65536 = comma i
  | otherwise = error "internal error: instruction doesn't fit in 16 bits"

jmp a | a < 8192 = comma a
      | otherwise = error "internal error: jump destination out of range"

asm tops = forM_ tops run

main = do
  [source, dest] <- getArgs
  pr <- parseFromFile sourceFile source

  case pr of
    Left e -> putStrLn $ show e
    Right tops -> do
      let (r, s) = runState (runExceptT $ runAsm $ asm tops) def
      case r of
        Left str -> putStrLn $ "Error: " ++ str
        Right st -> do
          let maxAddr = fromJust $ S.lookupMax $ M.keysSet $ asMem s

          putStrLn "ok"
          putStrLn $ "cells used: " ++ show (maxAddr + 1)

          forM_ [0 .. maxAddr] $ \a ->
            case M.lookup a (asMem s) of
              Just v  -> printf "  %04x %04x\n" (2 * a) v
              Nothing -> printf "  %04x ....\n" (2 * a)

          putStrLn "Symbols:"
          forM_ (M.toList $ asDict s) $ \(n, d) -> case d of
            Compiled a -> printf "  %04x %s\n" a n
            _ -> pure ()

          out <- openFile dest WriteMode
          forM_ [0 .. maxAddr] $ \a ->
            hPrintf out "%016b\n" $ fromMaybe 0xDEAD $ M.lookup a $ asMem s
          hClose out
