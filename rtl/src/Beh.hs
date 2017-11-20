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
  next

executeNormally :: (MonadState MS m, MonadReader IS m) => m OS
executeNormally = do
  inst <- view isMData
  if msb inst == 1
    then do -- Literal.
      t <- use msT
      msT .= zeroExtend (slice d14 d0 inst) -- load literal
      msDPtr += 1 -- push data stack
      next
        <&> osDOp . _2 .~ Just t -- flush old T value

    else case slice d14 d13 inst of
      0b00 -> do -- Jump.
        msPC .= zeroExtend (slice d12 d0 inst)
        fetch

      0b01 -> do -- Conditional jump.
        z <- (== 0) <$> use msT   -- test current T value for zero
        msDPtr -= 1               -- pop data stack
        assign msT =<< view isDData -- "
    
        pc' <- (+ 1) <$> use msPC
        msPC .= if z then zeroExtend $ slice d12 d0 inst else pc'
        fetch

      0b10 -> do -- Call.
        pc' <- (+ 1) <$> use msPC
        msPC .= zeroExtend (slice d12 d0 inst)
        msRPtr += 1  -- push return stack
        fetch
          <&> osROp . _2 .~ Just pc'

      0b11 -> do -- ALU.
        n <- view isDData
        r <- view isRData
        t <- use msT
        pc <- use msPC

        -- Bit 12: R -> PC
        msPC .= if slice d12 d12 inst /= 0
                  then r
                  else pc + 1

        -- Bit 7: T -> N
        let dop = if slice d7 d7 inst /= 0
                    then Just t
                    else Nothing

        -- Bit 6: T -> R
        let rop = if slice d6 d6 inst /= 0
                    then Just t
                    else Nothing
        -- Bit 5: N -> [T]
        let mwrite = if slice d5 d5 inst /= 0
                       then Just (t, n)
                       else Nothing

        let mread = if slice d4 d4 inst /= 0
                      then Just t
                      else Just (pc + 1)

        msLoadFlag .= (slice d4 d4 inst /= 0)

        depth <- use msDPtr

        msDPtr += signExtend (slice d1 d0 inst)
        msRPtr += signExtend (slice d3 d2 inst)

        msT .= case slice d11 d8 inst of
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
    <&> osMRead .~ Just (pc ++# 0)

outputs :: (MonadState MS m) => m OS
outputs = do
  dsp <- use msDPtr
  rsp <- use msRPtr
  pure $ OS Nothing Nothing (dsp, Nothing) (rsp, Nothing)
