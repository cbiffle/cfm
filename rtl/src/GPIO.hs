{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module GPIO where

import Clash.Prelude hiding (Word)

import CFM.Types

-- | A fixed-direction output port. Supports the following registers:
-- - +0: set output pins.
-- - +2: OR output pins. 1 bits will set pins, 0 bits have no effect.
-- - +4: NAND output pins. 1 bits will clear pins, 0 bits have no effect.
--
-- Reading from any address gets the current pin status.
outport :: (HasClockReset d g s)
        => Signal d (Maybe (BitVector 2, Maybe Word))
        -> ( Signal d Word
           , Signal d Word
           )
outport = unbundle . moore outportT outportO 0
  where
    outportT :: Word -> Maybe (BitVector 2, Maybe Word) -> Word
    -- Writes
    outportT v (Just (a, Just v')) = case a of
      0 -> v'
      1 -> v .|. v'
      2 -> v .&. complement v'
      _ -> v `xor` v'
    -- Reads or unselected
    outportT v _ = v

    outportO :: Word -> (Word, Word)
    outportO v = (v, v)

-- | An input port. This is currently rather specialized.
--
-- It reads a full word of input signals, which are registered and provided in
-- response to any read.
--
-- It also produces an interrupt on negative edges of bit 0. The interrupt
-- condition can be cleared by any write to the port's address space.
inport :: (HasClockReset d g s)
       => Signal d Word
       -> Signal d (Maybe (t, Maybe Word))
       -> ( Signal d Word
          , Signal d Bool
          )
inport = curry $ mooreB inportT id (0, False)
  where
    inportT :: (Word, Bool) -> (Word, Maybe (t, Maybe Word)) -> (Word, Bool)
    inportT (reg, irq) (port, req) = (port, irq')
      where
        irq' = case req of
          -- Clear the IRQ flag on any write.
          Just (_, Just _) -> False
          -- Otherwise, OR in the negative edge detector.
          _                -> irq || negedge
        negedge = unpack (lsb reg .&. complement (lsb port))
