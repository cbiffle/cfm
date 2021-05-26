{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module RTL.GPIO where

import Clash.Prelude
import RTL.IOBus (moorep)

import CFM.Types

-- | A fixed-direction output port. Supports the following registers:
-- - +0: set output pins.
-- - +2: OR output pins. 1 bits will set pins, 0 bits have no effect.
-- - +4: NAND output pins. 1 bits will clear pins, 0 bits have no effect.
--
-- Reading from any address gets the current pin status.
outport :: (HiddenClockResetEnable d)
        => Signal d (Maybe (BitVector 2, Maybe Cell))
        -> ( Signal d Cell
           , Signal d Cell
           )
outport = moorep outportT repeat id (pure ())
  where
    outportT :: Cell -> (Maybe (BitVector 2, Maybe Cell), ()) -> Cell
    -- Writes
    outportT v (Just (a, Just v'), _) = case a of
      0 -> v'
      1 -> v .|. v'
      2 -> v .&. complement v'
      _ -> v `xor` v'
    -- Reads or unselected
    outportT v _ = v

-- | An input port. This is currently rather specialized.
--
-- It reads a full word of input signals, which are registered and provided in
-- response to any read.
--
-- It also produces an interrupt on negative edges of bit 0. The interrupt
-- condition can be cleared by any write to the port's address space.
inport :: (KnownNat a, HiddenClockResetEnable d)
       => Signal d Cell
       -> Signal d (Maybe (BitVector a, Maybe Cell))
       -> ( Signal d Cell
          , Signal d Bool
          )
inport = moorep inportT (repeat . \(InportS x _) -> x) (\(InportS _ x) -> x)
  where
    inportT (InportS reg irq) (req, port) = InportS port irq'
      where
        irq' = case req of
          -- Clear the IRQ flag on any write.
          Just (_, Just _) -> False
          -- Otherwise, OR in the negative edge detector.
          _                -> irq || negedge
        negedge = bitCoerce (lsb reg .&. complement (lsb port))

data InportS = InportS Cell Bool deriving (Show, Generic, NFDataX)
instance Default InportS where def = InportS def False
