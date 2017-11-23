{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Str where

import Clash.Prelude hiding (Word, v)

import Types

datapath :: MS -> IS -> (MS, OS)
datapath (MS dptr rptr pc t lf) (IS m n r) =
  let inst = unpack m
      tmux = case inst of
            NotLit (ALU _ x _ _ _ _ _) -> x
            NotLit (JumpZ _)           -> N
            _                          -> T
      duringLoadElse :: t -> t -> t
      a `duringLoadElse` b = if lf then a else b

      stack ptr ~(d, wr) = (ptr, Nothing) `duringLoadElse`
                           (ptr + signExtend d, wr)
      (dptr', dop) = stack dptr $ case inst of
            Lit _                       -> (1, Just t)
            NotLit (ALU _ _ tn _ _ _ d) -> (d, if tn then Just t else Nothing)
            NotLit (JumpZ _)            -> (-1, Nothing)
            _                           -> (0, Nothing)
      (rptr', rop) = stack rptr $ case inst of
            NotLit (Call _)             -> (1, Just ((pc + 1) ++# 0))
            NotLit (ALU _ _ _ tr _ d _) -> (d, if tr then Just t else Nothing)
            _                           -> (0, Nothing)

      lf' = not lf && case inst of
            NotLit (ALU _ MemAtT _ _ _ _ _) -> True
            _                               -> False
      pc' = pc `duringLoadElse` case inst of
            NotLit (Jump tgt)             -> zeroExtend tgt
            NotLit (Call tgt)             -> zeroExtend tgt
            NotLit (JumpZ tgt) | t == 0   -> zeroExtend tgt
            NotLit (ALU True _ _ _ _ _ _) -> slice d15 d1 r
            _                             -> pc + 1

      (lessThan, nMinusT) = split (n `minus` t)
      signedLessThan | msb t /= msb n = msb n
                     | otherwise = lessThan
      t' = case tmux of
            T        -> t
            N        -> n
            TPlusN   -> t + n
            TAndN    -> t .&. n
            TOrN     -> t .|. n
            TXorN    -> t `xor` n
            NotT     -> complement t
            NEqT     -> signExtend $ pack $ n == t
            NLtT     -> signExtend signedLessThan
            NRshiftT -> n `shiftR` fromIntegral t
            NMinusT  -> nMinusT
            R        -> r
            MemAtT   -> errorX "value will be loaded next cycle"
            NLshiftT -> n `shiftL` fromIntegral t
            Depth    -> zeroExtend dptr
            NULtT    -> signExtend lessThan

  in ( MS { _msDPtr = dptr'
          , _msRPtr = rptr'
          , _msPC = pc'
          , _msLoadFlag = lf'
          , _msT = m `duringLoadElse` case inst of
                Lit v -> zeroExtend v
                _     -> t'
          }
     , OS { _osMWrite = Nothing `duringLoadElse` case inst of
                NotLit (ALU _ _ _ _ True _ _) -> Just (slice d15 d1 t, n)
                _                             -> Nothing
          , _osMRead = if lf' then slice d15 d1 t else pc'
          , _osDOp = (dptr', dop)
          , _osROp = (rptr', rop)
          }
     )
