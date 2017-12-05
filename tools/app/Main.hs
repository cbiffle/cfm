{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Default
import Text.Printf
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (openFile, hClose, IOMode(WriteMode))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Control.Monad (forM_)

import Parser
import Assembler

main :: IO ()
main = do
  [source, dest] <- getArgs
  pr <- parseSourceFile source

  case pr of
    Left e -> do
      print e
      exitWith (ExitFailure 1)
    Right tops -> do
      let (r, s) = runState (runExceptT $ runAsm $ asm tops) def
      case r of
        Left str -> do
          putStrLn $ "Error: " ++ str
          exitWith (ExitFailure 1)
        Right () -> do
          let maxAddr = fromJust $ S.lookupMax $ M.keysSet $ asMem s

          putStrLn "ok"
          putStrLn $ "cells used: " ++ show (maxAddr + 1)

          forM_ [0 .. maxAddr] $ \a ->
            case M.lookup a (asMem s) of
              Just v  -> printf "  %04x %04x   %s\n" (2 * a) (deval v) (dis v)
              Nothing -> printf "  %04x ....\n" (2 * a)

          putStrLn "Symbols:"
          forM_ (M.toList $ asDict s) $ \(n, d) -> case d of
            Compiled a -> printf "  %04x %s\n" (2 * a) n
            _ -> pure ()

          out <- openFile dest WriteMode
          forM_ [0 .. maxAddr] $ \a ->
            hPrintf out "%04x\n" $ maybe 0xDEAD deval $ M.lookup a $ asMem s
          hClose out

dis :: Val -> String
dis (Data v) | v < 0x8000 = show v
             | otherwise = "raw: " ++ show v
dis (I i) = show i
