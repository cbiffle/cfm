{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Structural model for ICE40 synthesis.
module RTL.Str where

import Clash.Prelude hiding (read)

import CFM.Inst
import CFM.Types
import RTL.Shifter
import RTL.CoreInterface

-- | Combinational datapath for CFM core.
datapath :: MS -> IS -> (MS, OS)
datapath (MS dptr rptr pc t bs lastSpace) (IS m i n r) =
  let -- things that do not depend on instruction decoding
      instValid = bs == BusFetch
      -- Factored pattern: a mux that depends on the state of the bus,
      -- for doing something different during a load/store cycle.
      duringLoadStoreElse :: t -> t -> t
      a `duringLoadStoreElse` b = if instValid then b else a

      -- Common stack logic. Preserves stack during load, otherwise applies the
      -- delta and write operation.
      stack :: SP -> (SDelta, Maybe Cell) -> (SP, SDelta, Maybe Cell)
      stack ptr ~(d, wr) = (ptr, 0, Nothing) `duringLoadStoreElse`
                           (ptr + pack (signExtend d), d, wr)

      pc1 = pc + 1
      -- Magnitude comparison and subtraction are implemented in terms of each
      -- other.
      (lessThan, nMinusT) = split @_ @1 (n `minus` t)
      signedLessThan | msb t /= msb n = msb n
                     | otherwise = lessThan

      space = unpack $ msb t
      loadResult = case lastSpace of
            MSpace -> m
            ISpace -> i
  in let  -- things that *do* depend on instruction decoding
      inst = unpack m
      -- Data and return stack interface.
      (dptr', ddlt, dop) = stack dptr $ case inst of
            Lit _                         -> (1, Just t)
            NotLit (ALU _ _ tn _ _ _ _ d) -> (d, if tn then Just t else Nothing)
            NotLit (JumpZ _)              -> (-1, Nothing)
            _                             -> (0, Nothing)
      (rptr', rdlt, rop) = stack rptr $ case inst of
            NotLit (Call _)               -> (1, Just (low ++# pc1 ++# low))
            NotLit (ALU _ _ _ tr _ _ d _) -> (d, if tr then Just t else Nothing)
            _                             -> (0, Nothing)
      -- Register updates other than the ALU
      bs' = case inst of
            _ | not instValid                 -> BusFetch
            NotLit (ALU _ MemAtT _ _ _ _ _ _) -> BusData True
            NotLit (ALU _ _ _ _ True _ _ _)   -> BusData False
            _                                 -> BusFetch

      pc' = pc `duringLoadStoreElse` case inst of
            NotLit (Jump tgt)               -> zeroExtend tgt
            NotLit (Call tgt)               -> zeroExtend tgt
            NotLit (JumpZ tgt) | t == 0     -> zeroExtend tgt
            NotLit (ALU True _ _ _ _ _ _ _) -> slice d14 d1 r
            _                               -> pc1
      -- The ALU mux.
      tmux = case inst of
            NotLit (ALU _ x _ _ _ _ _ _) -> x
            NotLit (JumpZ _)             -> N   -- pops data stack
            _                            -> T   -- leaves data stack unchanged
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
            Depth    -> rptr ++# dptr
            NULtT    -> signExtend lessThan

      mreq = case inst of
            _ | not instValid                 -> Just (pc', Nothing)
            NotLit (ALU _ MemAtT _ _ _ _ _ _) ->
              if space == MSpace
                then Just (slice d14 d1 t, Nothing)
                else Nothing
            NotLit (ALU _ _ _ _ True _ _ _)   ->
              if space == MSpace
                then Just (slice d14 d1 t, Just n)
                else Nothing
            _ -> Just (pc', Nothing)

      ireq = Nothing `duringLoadStoreElse` case inst of
            NotLit (ALU _ MemAtT _ _ _ _ _ _) | space == ISpace ->
                Just (slice d14 d1 t, Nothing)
            NotLit (ALU _ _ _ _ True _ _ _)   | space == ISpace->
                Just (slice d14 d1 t, Just n)
            _ -> Nothing

  in ( MS { _msDPtr = dptr'
          , _msRPtr = rptr'
          , _msPC = pc'
          , _msBusState = bs'
          , _msLastSpace = space
          , _msT = case bs of
              BusData True -> loadResult
              BusData False -> t
              BusFetch -> case inst of
                Lit v -> zeroExtend v
                _     -> t'
          }
     , OS { _osMReq = mreq
          , _osIReq = ireq
          , _osDOp = (dptr', ddlt, dop)
          , _osROp = (rptr', rdlt, rop)
          , _osFetch = bs' == BusFetch
          }
     )
