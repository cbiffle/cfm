{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- | Structural model for ICE40 synthesis.
module Str where

import Clash.Prelude hiding (Word, read)

import Inst
import Shifter
import Types
import CoreInterface

-- | Combinational datapath for CFM core.
datapath :: MS -> IS -> (MS, OS)
datapath (MS dptr rptr pc t lf lastSpace) (IS m i n r) =
  let -- instruction decoding
      inst = unpack m
      tmux = case inst of
            NotLit (ALU _ x _ _ _ _ _ _) -> x
            NotLit (JumpZ _)             -> N   -- pops data stack
            _                            -> T   -- leaves data stack unchanged

      -- Factored pattern: a mux that depends on the state of the Load Flag
      -- register, for doing something different during a load cycle.
      duringLoadElse :: t -> t -> t
      a `duringLoadElse` b = if lf then a else b

      -- Common stack logic. Preserves stack during load, otherwise applies the
      -- delta and write operation.
      stack :: SP -> (SDelta, Maybe Word) -> (SP, SDelta, Maybe Word)
      stack ptr ~(d, wr) = (ptr, 0, Nothing) `duringLoadElse`
                           (ptr + pack (signExtend d), d, wr)

      -- Data and return stack interface.
      (dptr', ddlt, dop) = stack dptr $ case inst of
            Lit _                         -> (1, Just t)
            NotLit (ALU _ _ tn _ _ _ _ d) -> (d, if tn then Just t else Nothing)
            NotLit (JumpZ _)              -> (-1, Nothing)
            _                             -> (0, Nothing)
      (rptr', rdlt, rop) = stack rptr $ case inst of
            NotLit (Call _)               -> (1, Just (low ++# (pc + 1) ++# low))
            NotLit (ALU _ _ _ tr _ _ d _) -> (d, if tr then Just t else Nothing)
            _                             -> (0, Nothing)

      -- Register updates other than the ALU
      lf' = not lf && case inst of
            NotLit (ALU _ MemAtT _ _ _ _ _ _) -> True
            _                                 -> False
      pc' = pc `duringLoadElse` case inst of
            NotLit (Jump tgt)               -> zeroExtend tgt
            NotLit (Call tgt)               -> zeroExtend tgt
            NotLit (JumpZ tgt) | t == 0     -> zeroExtend tgt
            NotLit (ALU True _ _ _ _ _ _ _) -> slice d14 d1 r
            _                               -> pc + 1

      -- The ALU. Magnitude comparison is implemented in terms of subtraction.
      -- This means we get subtraction for free.
      (lessThan, nMinusT) = split @_ @1 (n `minus` t)
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
              -- Note: equality could be implemented more compactly by testing
              -- the subtractor output against zero. However, the subtractor is
              -- one of the longer paths through the ALU, and testing its
              -- result adds to that, reducing speed.
            NLtT     -> signExtend signedLessThan
            NRshiftT -> n `rightShift` slice d3 d0 t
            NMinusT  -> nMinusT
            R        -> r
            MemAtT   -> errorX "value will be loaded next cycle"
            NLshiftT -> n `leftShift` slice d3 d0 t
            Depth    -> zeroExtend dptr
            NULtT    -> signExtend lessThan

      space = unpack $ msb t
      write = Nothing `duringLoadElse` case inst of
            NotLit (ALU _ _ _ _ True _ _ _) -> Just (space, slice d14 d1 t, n)
            _                               -> Nothing
      busReq = if lf'
                  then case space of
                    MSpace -> MReq (slice d14 d1 t) write
                    ISpace -> IReq (slice d14 d1 t)
                  else MReq pc' write

      loadResult = case lastSpace of
            MSpace -> m
            ISpace -> i

  in ( MS { _msDPtr = dptr'
          , _msRPtr = rptr'
          , _msPC = pc'
          , _msLoadFlag = lf'
          , _msLastSpace = space
          , _msT = loadResult `duringLoadElse` case inst of
                Lit v -> zeroExtend v
                _     -> t'
          }
     , OS { _osBusReq = busReq
          , _osDOp = (dptr', ddlt, dop)
          , _osROp = (rptr', rdlt, rop)
          }
     )
