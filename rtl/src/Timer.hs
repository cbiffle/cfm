{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Timer where

import Clash.Prelude hiding (Word)
import Control.Arrow (first)

import CFM.Types

type MatchCount = 2
type CtrWidth = 13

type Addr = BitVector (CLog 2 (2 + MatchCount))
type Ctr = BitVector CtrWidth

-- | A simple timer with match registers.
--
-- This is a free-running timer that counts up every core cycle. Its I/O bus
-- interface contains four registers, each of which can be read and written:
--
-- 0: the counter value. 13 bits, top bits read as zero / writes ignored.
-- Increments on any cycle that it isn't written.
--
-- 1: status register. Each bit contains the match flag for the corresponding
-- match register (currently two implemented). Write 1 to clear.
--
-- 2+: match registers. When the counter value matches the contents of a match
-- register, the corresponding match bit in the status register will be set.
--
-- The match bits in the status register are also exposed as IRQs.
timer :: (HasClockReset d g s)
      => Signal d (Maybe (Addr, Maybe Word))
      -> ( Vec MatchCount (Signal d Bool)
         , Signal d Word
         )
timer = first unbundle . unbundle . moore timerT timerO (0, repeat False, repeat 0, 0)

-- | Transition function for timer state.
timerT :: (Ctr, Vec MatchCount Bool, Vec MatchCount Ctr, Addr)
       -> Maybe (Addr, Maybe Word)
       -> (Ctr, Vec MatchCount Bool, Vec MatchCount Ctr, Addr)
timerT (ctr, irqs, matches, lastAddr) req = (ctr', irqs', matches', lastAddr')
  where
    ctr' = case req of
      Just (0, Just v) -> truncateB v
      _                -> ctr + 1
    irqs' = case req of
      Just (1, Just v) -> zipWith (&&) irqs
                                       (unpack $ complement $ slice d1 d0 v)
      _                -> zipWith (||) irqs $ map (== ctr) matches
    matches' = flip map indicesI $ \i -> case req of
      Just (n, Just v) | n == fromIntegral i + 2 -> truncateB v
      _                                          -> matches !! i
    lastAddr' = case req of
      Just (a, _) -> a
      _           -> lastAddr

-- | Exposes the IRQs and the delayed register reads.
timerO :: (Ctr, Vec MatchCount Bool, Vec MatchCount Ctr, Addr)
       -> (Vec MatchCount Bool, Word)
timerO (ctr, irqs, matches, lastAddr) = (irqs, rs !! lastAddr)
  where
    rs = zeroExtend ctr :> zeroExtend (pack irqs) :> map zeroExtend matches
