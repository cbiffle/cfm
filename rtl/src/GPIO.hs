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
outport :: (KnownNat n, HasClockReset d g s)
        => Signal d (Maybe (BitVector (2 + n), Maybe Word))
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
       -> Signal d (Maybe (BitVector n, Maybe Word))
       -> Signal d Word
inport port _ = resp
  where
    resp = register 0 port
