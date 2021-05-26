{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RTL.Timer where

import Clash.Prelude
import Control.Arrow (second)

import CFM.Types
import RTL.IOBus (moorep)

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
timer :: (HiddenClockResetEnable d)
      => Signal d (Maybe (Addr, Maybe Cell))
      -> ( Signal d Cell
         , Vec MatchCount (Signal d Bool)
         )
timer = second unbundle . moorep timerT timerR timerO (pure ())
  where
    timerO (TimS _ irqs _) = irqs

data TimS = TimS Ctr (Vec MatchCount Bool) (Vec MatchCount Ctr)
  deriving (Generic, NFDataX)

instance Default TimS where
  def = TimS 0 (repeat False) (repeat 0)

timerT :: TimS -> (Maybe (Addr, Maybe Cell), ()) -> TimS
timerT (TimS ctr irqs matches) (req, _) = TimS ctr' irqs' matches'
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

timerR :: TimS -> Vec 4 Cell
timerR (TimS ctr irqs matches) =
  zeroExtend ctr :>
  zeroExtend (pack irqs) :>
  map zeroExtend matches
