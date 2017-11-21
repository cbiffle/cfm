{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Str where

import Clash.Prelude hiding (Word, cycle)

import Types

cycle' :: MS -> IS -> (OS, MS)
cycle' (MS dptr rptr pc t lf) (IS m n r) =
  let inst = unpack m
      dptr' | lf = dptr
            | otherwise = dptr + signExtend (case inst of
                Lit _ -> 1
                NotLit (ALU _ _ _ _ _ _ d) -> d
                NotLit (JumpZ _) -> -1
                _ -> 0)
      rptr' | lf = rptr
            | otherwise = rptr + signExtend (case inst of
                NotLit (Call _) -> 1
                NotLit (ALU _ _ _ _ _ d _) -> d
                _ -> 0)
      lf' = not lf && case inst of
              NotLit (ALU _ 12 _ _ _ _ _) -> True
              _ -> False
      pc' | lf = pc
          | otherwise = case inst of
              NotLit (Jump tgt) -> zeroExtend tgt
              NotLit (Call tgt) -> zeroExtend tgt
              NotLit (JumpZ tgt) | t == 0 -> zeroExtend tgt
              NotLit (ALU True _ _ _ _ _ _) -> slice d15 d1 r
              _ -> pc + 1
      tmux = case inst of
              NotLit (ALU _ x _ _ _ _ _) -> x
              NotLit (JumpZ _) -> 1
              _ -> 0
      (lessThan, nMinusT) = split (n `minus` t)
      signedLessThan | msb t /= msb n = msb n
                     | otherwise = lessThan
      t'mux = case tmux of
                0 -> t
                1 -> n
                2 -> t + n
                3 -> t .&. n
                4 -> t .|. n
                5 -> t `xor` n
                6 -> complement t
                7 -> signExtend $ pack $ n == t
                8 -> signExtend signedLessThan
                9 -> n `shiftR` fromIntegral t
                10 -> nMinusT
                11 -> r
                12 -> errorX "value will be loaded next cycle"
                13 -> n `shiftL` fromIntegral t
                14 -> zeroExtend dptr
                15 -> signExtend lessThan
      dop = if lf then Nothing else case inst of
              Lit _ -> Just t
              NotLit (ALU _ _ True _ _ _ _) -> Just t
              _ -> Nothing
      rop = if lf then Nothing else case inst of
              NotLit (Call _) -> Just ((pc + 1) ++# 0)
              NotLit (ALU _ _ _ True _ _ _) -> Just t
              _ -> Nothing
  in ( OS { _osMWrite =
              if lf then Nothing else case inst of
                NotLit (ALU _ _ _ _ True _ _) -> Just (slice d15 d1 t, n)
                _ -> Nothing
          , _osMRead = if lf' then slice d15 d1 t else pc'
          , _osDOp = (dptr', dop)
          , _osROp = (rptr', rop)
          }
      , MS { _msDPtr = dptr'
           , _msRPtr = rptr'
           , _msPC = pc'
           , _msLoadFlag = lf'
           , _msT = if lf then m else case inst of
                      Lit v -> zeroExtend v
                      _ -> t'mux
           }
      )
