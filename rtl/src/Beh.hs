{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
module Beh where

import Clash.Prelude hiding (Word, cycle)
import GHC.Generics

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader

import Types

cycle' :: MS -> IS -> (OS, MS)
cycle' m = runReader (runStateT cycle m)

cycle :: (MonadState MS m, MonadReader IS m) => m OS
cycle = do
  lf <- use msLoadFlag
  if lf
    then finishLoad
    else executeNormally

finishLoad :: (MonadState MS m, MonadReader IS m) => m OS
finishLoad = do
  assign msT =<< view isMData
  msLoadFlag .= False
  fetch

executeNormally :: (MonadState MS m, MonadReader IS m) => m OS
executeNormally = do
  inst <- views isMData unpack
  case inst of
    Lit v -> do
      t <- use msT
      msT .= zeroExtend v -- load literal
      msDPtr += 1 -- push data stack
      next
        <&> osDOp . _2 .~ Just t -- flush old T value

    NotLit (Jump tgt) -> do
      msPC .= zeroExtend tgt
      fetch

    NotLit (JumpZ tgt) -> do
      z <- (== 0) <$> use msT   -- test current T value for zero
      msDPtr -= 1               -- pop data stack
      assign msT =<< view isDData -- "
    
      pc' <- (+ 1) <$> use msPC
      msPC .= if z then zeroExtend tgt else pc'
      fetch

    NotLit (Call tgt) -> do
      pc' <- (+ 1) <$> use msPC
      msPC .= zeroExtend tgt
      msRPtr += 1  -- push return stack
      fetch
        <&> osROp . _2 .~ Just pc'

    NotLit (ALU rpc t' tn tr nm mt rd dd) -> do
      n <- view isDData
      r <- view isRData
      t <- use msT
      pc <- use msPC

      -- Bit 12: R -> PC
      let pc' = if rpc
                  then r
                  else pc + 1

      msPC .= pc'

      -- Bit 7: T -> N
      let dop = if tn
                  then Just t
                  else Nothing

      -- Bit 6: T -> R
      let rop = if tr
                  then Just t
                  else Nothing
      -- Bit 5: N -> [T]
      let mwrite = if nm
                     then Just (t, n)
                     else Nothing

      let mread = if mt
                    then t
                    else pc'

      msLoadFlag .= mt

      depth <- use msDPtr

      msDPtr += signExtend dd
      msRPtr += signExtend rd

      msT .= case t' of
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
        14 -> zeroExtend depth
        15 -> signExtend $ pack $ n < t

      outputs
        <&> osMRead .~ mread
        <&> osDOp . _2 .~ dop
        <&> osROp . _2 .~ rop
        <&> osMWrite .~ mwrite

next :: (MonadState MS m) => m OS
next = do
  msPC += 1
  fetch

fetch :: (MonadState MS m) => m OS
fetch = do
  pc <- use msPC
  outputs
    <&> osMRead .~ pc ++# 0

outputs :: (MonadState MS m) => m OS
outputs = do
  dsp <- use msDPtr
  rsp <- use msRPtr
  pure $ OS Nothing 0 (dsp, Nothing) (rsp, Nothing)
