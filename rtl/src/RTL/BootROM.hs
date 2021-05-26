{-# LANGUAGE NoImplicitPrelude #-}

-- | A simple ROM for loading a known program at boot.
module RTL.BootROM where

import Clash.Prelude
import CFM.Types

-- | The Boot ROM is a simple way of getting a known program visible in memory
-- at reset, and is arguably a giant hack.
--
-- For systems using FPGA BRAM as RAM, the BRAM can be initialized in the
-- bitstream with an arbitrary program. Such systems don't need a Boot ROM.
--
-- For systems using external RAM, we have a classic bootstrapping problem: the
-- CPU wants to execute from address 0 at reset, but external memory at address
-- 0 contains undefined goo. The Boot ROM fixes this.
--
-- This circuit monitors memory requests and fetch strobes from the CPU, and
-- intercepts the memory return bus. At reset, it interposes itself, answering
-- all read requests. This ensures that a program stored in the Boot ROM gets
-- control at reset.
--
-- It still lets requests through to normal memory, so that the boot program can
-- fill in RAM with another program (loaded perhaps from Flash).
--
-- To minimize its impact on the running system, the Boot ROM does not present
-- an interface in I/O space, which raises the question of how to turn it off
-- when bootstrap is complete. The answer is perhaps too clever: the final act
-- of any bootstrap program is to jump to address 0. The ROM interposer circuit
-- monitors the CPU fetch bus, and on the *second* such jump (the first having
-- activated the boot program at reset), the interposer disables itself and
-- normal RAM is exposed.
bootROM :: (HiddenClockResetEnable d, KnownNat a)
        => SNat n
          -- ^ Size of the ROM.
        -> FilePath
          -- ^ Synthesis-time path to the binary init file.
        -> Signal d (Maybe (BitVector a, Maybe Cell))
          -- ^ Memory request from CPU.
        -> Signal d Bool
          -- ^ Fetch strobe from CPU, needed to detect execution at 0.
        -> Signal d Cell
          -- ^ Memory response from normal, non-boot devices.
        -> Signal d Cell
          -- ^ Altered response inserting boot ROM where appropriate.
bootROM n file mreq fetch = mux shadowFlag romdata
  where
    shadowFlag = regEn True fetch0 $
                 regEn True fetch0 $
                 pure False
    fetch0 = liftA2 (&&) fetch $ mreq .==. pure (Just (0, Nothing))
    romdata = blockRamFile n file (maybe undefined fst <$> mreq) (pure Nothing)
