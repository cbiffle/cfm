{-# LANGUAGE NoImplicitPrelude #-}

-- | External SRAM interface.
module RTL.SRAM where

import Clash.Prelude
import Data.Maybe (fromMaybe, isJust)

import CFM.Types

-- | Simple interface to external SRAM. This is only part of the circuit,
-- Verilog support code is required.
--
-- This interface assumes SRAM with an access time significantly shorter than
-- our clock cycle. It registers the memory access parameters during the initial
-- access cycle, and presents them to the SRAM during the response cycle. The
-- SRAM must therefore do its entire access in less than a cycle.
--
-- To ensure that outputs are held stable during writes, and that back-to-back
-- writes work, this interface requires an external circuit to generate the SRAM
-- /WE signal. The easiest method is AND-ing this circuit's write output with an
-- out-of-phase version of the core clock, producing a write pulse of 1/2 clock
-- period offset within the cycle.
--
-- It currently seems easier to do this outside of Clash than within it.
extsram :: (HasClockReset d g s)
        => Signal d (Maybe (BitVector a, Maybe Cell)) -- ^ Memory request.
        -> ( Signal d (BitVector a)
           , Signal d Bool
           , Signal d Cell
           )  -- ^ SRAM address, SRAM write, host-to-SRAM data
extsram = unbundle . moore extsramT extsramO def
  where
    -- State type is simply (BitVector a, Maybe Cell)
    extsramT s = fromMaybe (fst s, Nothing)
    extsramO s = (fst s, isJust (snd s), fromMaybe rdu (snd s))
    rdu = errorX "Host-to-SRAM data undefined during read"
