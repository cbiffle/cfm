{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module GPIO where

import Clash.Prelude hiding (Word)

import Types

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
outport cmd = (resp, outs)
  where
    update o (Just (a, Just v)) = case slice d1 d0 a of
      0 -> v
      1 -> o .|. v
      _ -> o .&. complement v
    update o _ = o
    outs = register 0 $ update <$> outs <*> cmd
    resp = outs

inport :: (HasClockReset d g s)
       => Signal d Word
       -> Signal d (Maybe (t, Maybe Word))
       -> ( Signal d Word
          , Signal d Bool
          )
inport port cmd = (resp, irq)
  where
    resp = register 0 port
    negedge = unpack <$> ((.&.) <$> (lsb <$> resp) <*> (complement . lsb <$> port))
    irq = register False $ update <$> negedge <*> cmd
    update _ (Just (_, Just _)) = False
    update x _ = x
