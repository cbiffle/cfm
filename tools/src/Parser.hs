{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Parser
  ( parseSourceFile
  , sourceFile
  , AsmTop(..)
  , AsmFrag(..)
  , LoopEnd(..)
  ) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Data.Char (isSpace)

parseSourceFile :: FilePath -> IO (Either ParseError [AsmTop])
parseSourceFile = parseFromFile sourceFile

-- | Forms that can appear at the top level of an assembly source file.
data AsmTop = Colon String [AsmFrag]
              -- ^ A colon definition, consisting of a name and a body.
            | Constant String
              -- ^ A constant definition. The value of the constant is implicit,
              -- and will be taken from the stack during assembly.
            | Variable String
              -- ^ A variable definition.
            | OrgDirective
              -- ^ An org directive. The address is implicit, and will be taken
              -- from the stack during assembly.
            | ALUPrim String
              -- ^ A named ALU primitive instruction. The bitwise encoding of
              -- the instruction will be taken from the stack during assembly.
            | Interp AsmFrag
              -- ^ Arbitrary code for interpretation. Words (including numbers)
              -- used outside colon definitions, which are not any of the forms
              -- above, fall into this category.
            deriving (Show)

-- | Forms that can appear within a definition, or at top level during
-- interpretation.
data AsmFrag = Word String
                -- ^ A word, including numbers.
             | Comment String
                -- ^ A line or block comment.
             | CharLit Char
                -- ^ A @[CHAR]@ literal
             | Begin [AsmFrag] LoopEnd
                -- ^ A begin-loop, consisting of a body and a loop-ending type.
             | If [AsmFrag] (Maybe [AsmFrag])
                -- ^ An if-then with optional else clause.
             | FusionBreak
                -- ^ Explicitly prevents fusion of two adjacent instructions.
                -- Useful mostly for nefarious purposes.
             | Tick String
                -- ^ Address of a word as a literal.
             deriving (Show)

-- | A loop ending.
data LoopEnd = Again
                -- ^ begin ... again, an infinite loop
             | Until
                -- ^ begin ... until
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

frag = comment <|> loop <|> ifThen <|> charLit <|> fusionBreak <|> tick <|> word

comment = Comment <$> (do sic "\\"
                          c <- many (noneOf "\n")
                          newline *> maybeWs
                          pure c
                       <|> sic "(" *> many (noneOf ")") <* char ')' <* maybeWs)
                  <?> "comment"

loop = uncurry Begin <$> (sic "begin" *> frag `manyTill'` loopEnd)
       <?> "loop"

loopEnd = (sic "again" >> pure Again)
      <|> (sic "until" >> pure Until)

ifThen = uncurry If <$> (sic "if" *> frag `manyTill'` thenOrElse)
          <?> "if-then"

thenOrElse = (sic "then" >> pure Nothing)
         <|> (sic "else" >> Just <$> frag `manyTill` sic "then")

charLit = CharLit . head <$> (sic "[char]" *> lexeme)

fusionBreak = sic "|" >> pure FusionBreak

tick = Tick <$> (sic "[']" *> lexeme)

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
