{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
module Str where

import Clash.Prelude hiding (Word, cycle)
import GHC.Generics

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader

import Types

cycle' :: MS -> IS -> (OS, MS)
cycle' m = runReader (runStateT cycle m)

cycle :: (MonadReader IS m, MonadState MS m) => m OS
cycle = do
  IS m n r <- ask
  MS dptr rptr pc t lf <- get
  let inst = unpack m

  let dptr' = if lf
                then dptr
                else dptr + signExtend (case inst of
                       Lit _ -> 1
                       NotLit (ALU _ _ _ _ _ _ _ d) -> d
                       NotLit (JumpZ _) -> -1
                       _ -> 0)
      rptr' = if lf
                then rptr
                else rptr + signExtend (case inst of
                       NotLit (Call _) -> 1
                       NotLit (ALU _ _ _ _ _ _ d _) -> d
                       _ -> 0)
      lf' = not lf && case inst of
              NotLit (ALU _ _ _ _ _ mt _ _) -> mt
              _ -> False
      pc' = if lf then pc else case inst of
              NotLit (Jump tgt) -> zeroExtend tgt
              NotLit (Call tgt) -> zeroExtend tgt
              NotLit (JumpZ tgt) | t == 0 -> zeroExtend tgt
              NotLit (ALU True _ _ _ _ _ _ _) -> r
              _ -> pc + 1
      t' = if lf then m else case inst of
            Lit v -> zeroExtend v
            NotLit (JumpZ _) -> n
            NotLit (ALU _ tmux _ _ _ _ _ _) -> case tmux of
              0 -> t
              1 -> n
              2 -> t + n
              3 -> t .&. n
              4 -> t .|. n
              5 -> t `xor` n
              6 -> complement t
              7 -> signExtend $ pack $ n == t
              8 -> signExtend $ pack $ unpack @(Signed 16) n < unpack t
              9 -> n `shiftR` fromIntegral t
              10 -> t - 1
              11 -> r
              12 -> errorX "RESERVED"
              13 -> n `shiftL` fromIntegral t
              14 -> zeroExtend dptr
              15 -> signExtend $ pack $ n < t
            _ -> t
      mread = if lf' then t else pc'
      dop = if lf then Nothing else case inst of
              Lit _ -> Just t
              NotLit (ALU _ _ True _ _ _ _ _) -> Just t
              _ -> Nothing
      rop = if lf then Nothing else case inst of
              NotLit (Call _) -> Just (pc + 1)
              NotLit (ALU _ _ _ True _ _ _ _) -> Just t
              _ -> Nothing

      mwrite = if lf then Nothing else case inst of
                NotLit (ALU _ _ _ _ True _ _ _) -> Just (t, n)
                _ -> Nothing

  unless lf $ do
    msDPtr .= dptr'
    msRPtr .= rptr'
    msPC .= pc'
  msLoadFlag .= lf'
  msT .= t'

  pure OS
    { _osMWrite = mwrite
    , _osMRead = mread
    , _osDOp = (dptr', dop)
    , _osROp = (rptr', rop)
    }
