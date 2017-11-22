{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals #-}
module Main where

import Text.Parsec hiding (spaces, State)
import Text.Parsec.String
import Text.Printf

import Data.Bits
import Data.Default
import Data.Char (isSpace)
import Data.Word (Word16)
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

import Control.Monad.State
import Control.Monad.Except

data AsmTop = Colon String [AsmFrag]
            | Constant String
            | Variable String
            | Interp AsmFrag
            deriving (Show)

data AsmFrag = Word String
             | Comment String
             deriving (Show)

sourceFile = skipMany space >> many top <* eof

top = label colon "colon definition"
  <|> label constant "constant definition"
  <|> label variable "variable definition"
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

interp = Interp <$> frag

frag = comment <|> word

comment = Comment <$> (do sic "\\"
                          c <- many (noneOf "\n")
                          (newline *> skipMany space <|> eof)
                          pure c
                       <|> sic "(" *> many (noneOf ")") <* char ')')
                  <?> "comment"

word = Word <$> lexeme

sic :: String -> Parser ()
sic s = try (string s *> ws)

lexeme :: Parser String
lexeme = many1 (satisfy (not . isSpace)) <* ws <?> "word"

ws :: Parser ()
ws = (skipMany1 space *> pure ()) <|> eof

---------

data Def = Compiled Integer
         | InlineLit Integer
         | Immediate (Asm ())

instance Show Def where
  show (Compiled x) = "Compiled " ++ show x
  show (InlineLit x) = "InlineLit " ++ show x
  show (Immediate _) = "Immediate ..."

data AS = AS
  { asDict :: M.Map String Def
  , asStack :: [Integer]
  , asPos :: Integer
  , asMem :: [Word16]
  } deriving (Show)

instance Default AS where
  def = AS M.empty [] 0 []

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

here :: Asm Integer
here = gets asPos

create n d = modify $ \s -> s { asDict = M.insert n d (asDict s) }

find n = do
  md <- gets (M.lookup n . asDict)
  case md of
    Nothing -> case reads n of
      [(x, "")] -> pure (InlineLit x)
      _         ->throwError ("use of unknown word " ++ n)
    Just d -> pure d

comma v | 0 <= v && v < 65536 =
  modify $ \s -> s { asPos = asPos s + 1
                   , asMem = fromIntegral v : asMem s
                   }
comma _ = error "internal error: value passed to comma out of range"
  
run (Colon name body) = do
  p <- here
  create name $ Compiled p
  forM_ body $ \f -> case f of
    Comment _ -> pure ()
    Word w -> find w >>= compile

run (Constant name) = do
  v <- pop
  create name $ InlineLit v

run (Variable name) = do
  p <- here
  create name $ InlineLit p
  comma 0

run (Interp (Comment _)) = pure ()

run (Interp (Word s)) = do
  d <- find s
  case d of
    Immediate f -> f
    _ -> throwError ("word " ++ s ++ " cannot be used in interpreted code")

compile (Compiled a)
  | a < 8192 = comma a
  | otherwise = error "internal error: colon def out of range"

compile (InlineLit x)
  | 0 <= x && x < 32768 = comma (x .|. 0x8000)
  | -32768 <= x && x < 0 = comma (complement x .|. 0x8000)
  | otherwise = error "internal error: literal out of range"

compile (Immediate f) = f


prim n a = create n (Immediate a)

prims :: [(String, Asm ())]
prims =
  [ ("swap", comma 0b0110000110000000)
  , ("<",    comma 0b0110100000000011)
  ]

asm tops = do
  forM_ prims $ uncurry prim
  forM_ tops run

main = do
  args <- getArgs
  pr <- parseFromFile sourceFile (head args)
  case pr of
    Left e -> putStrLn $ show e
    Right tops -> do
      let (r, s) = runState (runExceptT $ runAsm $ asm tops) def
      case r of
        Left str -> putStrLn $ "Error: " ++ str
        Right st -> do
          putStrLn "ok"
          putStrLn $ "cells used: " ++ show (asPos s)
          forM_ (zip [0 :: Word16 ..] $ reverse (asMem s)) $ \(i, c) -> 
            printf "  %04x %04x\n" (2 * i) c
          putStrLn "Symbols:"
          forM_ (M.toList $ asDict s) $ \(n, d) -> case d of
            Compiled a -> printf "  %04x %s\n" a n
            _ -> pure ()
