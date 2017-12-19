{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module RTL.Common.Bits where

import Clash.Prelude

-- | Reverses the order of bits in a vector.
revbits :: (KnownNat n) => BitVector n -> BitVector n
revbits = pack . reverse . unpack @(Vec _ Bit)
  -- The Verilog generated for this version is pretty scary, but it synthesizes
  -- into a bundle of wires the way you'd expect.
