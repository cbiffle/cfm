module Main where

import System.Environment (getArgs)
import Data.List.Split (chunksOf)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Char (isSpace)

-- This is a tool for moving between old-skool Forth block sequences and
-- Unix-style text files.

-- | Pads a line with spaces to 64 characters. Rejects input lines of 63 or more
-- characters; >64 is an obvious error, but 64 exactly can cause confusing run-
-- together problems.
padLine :: String -> String
padLine s
  | length s > 63 = error $ "line too long: " ++ s
  | otherwise = s ++ replicate (64 - length s) ' '

-- | Converts a screen of 16 variable-length lines into a padded, concatenated
-- block representation. Each line must be 63 characters or less. It is an error
-- for the input to contain more than 16 lines.
emblock :: [String] -> String
emblock ss
  | length ss > 16 = error $ "Too many lines in block: " ++ show ss
  | otherwise = concatMap padLine (ss ++ replicate (16 - length ss) "")

-- | Converts a list of lines into a sequence of blocks. Lines must be no more
-- than 63 characters. Each block is made of 0-16 lines. Each block, including
-- the last, must be terminated by the literal line "---".
blocks :: [String] -> [String]
blocks ss = map emblock $ gather ss [] []
  where
    gather [] [] racc = reverse racc
    gather [] _ _ = error "missing final terminator"
    gather ("---" : rest) bacc racc = gather rest [] (reverse bacc : racc)
    gather (s : rest) bacc racc = gather rest (s : bacc) racc

dropModeline :: [String] -> [String]
dropModeline (s : ss) | "\\ vim:" `isPrefixOf` s = ss
dropModeline ss = ss

deblock :: String -> String -> [String]
deblock sep s
  | length s /= 1024 = error $ "unexpected block size: " ++ show (length s)
  | otherwise = let fullBlock = map (dropWhileEnd isSpace) (chunksOf 64 s)
                in dropWhileEnd null fullBlock ++ [sep]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pack", textFile, blockFile] -> do
      d <- readFile textFile
      let packed = concat $ blocks $ dropModeline $ lines d
      writeFile blockFile packed
    ["unpack", blockFile, textFile] -> do
      d <- readFile blockFile
      let bs = concatMap (deblock "---") $ chunksOf 1024 d
      writeFile textFile $ unlines bs
    ["unpack", "-r", blockFile, textFile] -> do
      d <- readFile blockFile
      let bs = concatMap (deblock "") $ chunksOf 1024 d
      writeFile textFile $ unlines bs
    _ -> error "unrecognized command line option"
