{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- | Memory Management Unit for accessing more than 32kiB of RAM.
module RTL.MMU where

import Clash.Prelude
import Control.Arrow (first)

import CFM.Types
import RTL.Common.Strobe
import RTL.IOBus
import RTL.Strobes

-- | A Memory Management Unit that divides an address space into an array of
-- equally-sized pages. The CPU-generated address is a "virtual" address; the
-- MMU translates these addresses into physical addresses of implemented RAM.
--
-- In practice, this occurs by routing the top 'v' bits of the virtual address
-- into a lookup table, producing a 'p' bit result. Thus CPU addresses of @v+n@
-- bits are mapped into physical addresses of @p+n@ bits. 'p' is typically
-- bigger than 'v' to allow access to more memory than the CPU can natively
-- address, but this doesn't have to be the case.
--
-- Instantiating 'mmu' defines the I/O interface and registers to hold the
-- page translation table. Its outputs include the contents of the current
-- mapping table, which can be fed to 'mmuMapper' to rewrite outgoing
-- addresses.
--
-- The MMU contains two sets of mapping registers, map 0 and map 1. At reset,
-- map 0 is active. The map can be switched by writing the Control/Status
-- register with any value. This is intended to be fused into a return
-- instruction, to effect an atomic jump-and-change-maps operation.
--
-- Interrupt entry causes an automatic switch back to map 0 at the start of
-- vector fetch, so that the vector is always fetched from map 0. When this
-- occurs, bit 1 in the Control/Status register will be set. If this bit is
-- still set when interrupts are re-enabled (the 'EnablingInterrupts' strobe),
-- the map will switch back.
--
-- Register set:
--
-- - +0: Control/Status. Any write toggles bit 0.
--   - Bit 0: active map.
--   - Bit 1: switched due to IRQ flag.
-- - +2: Map Pointer. Holds a 'v'-bit index of a map register.
-- - +4: Map 0 Access. Reads/writes the part of map 0 selected by Map Pointer.
-- - +6: Map 1 Access. Reads/writes the part of map 1 selected by Map Pointer.
-- - +8: Active Map flag in bit 0 (read/write).
-- - +A: Switched due to IRQ flag in bit 0 (read/write).
mmu :: forall v p ev ep d.
       ( HiddenClockResetEnable d
       , KnownNat v
       , KnownNat p
       , KnownNat ev
       , KnownNat ep
       , (ev + v) ~ Width
       , (v + ev) ~ Width
       , (ep + p) ~ Width
       , (p + ep) ~ Width
       )
    => Signal d (Maybe VectorFetchAddress)
    -> Signal d (Maybe EnablingInterrupts)
    -> Signal d (Maybe (BitVector 3, Maybe Cell))
    -> ( Signal d Cell
       , Signal d (Vec (2^v) (BitVector p))
       )
mmu vecfetchS enirqS = mealyp fT fR $ bundle (vecfetchS, enirqS)
  where
    fT :: S v p
       -> ( Maybe (BitVector 3, Maybe Cell)
          , (Maybe VectorFetchAddress, Maybe EnablingInterrupts)
          )
       -> (S v p, Vec (2^v) (BitVector p))
    fT (S map0 map1 mp m1a sirq) (req, (vecfetch, enirq)) = (s', activeMap)
      where
        s' = S map0' map1' mp' m1a' sirq'

        mp' | Just (1, Just v) <- req = truncateB v
            | otherwise = mp

        map0' | Just (2, Just v) <- req = replace mp (truncateB v) map0
              | otherwise = map0

        map1' | Just (3, Just v) <- req = replace mp (truncateB v) map1
              | otherwise = map1

        m1a' | fromStrobe vecfetch = False
             | fromStrobe enirq && sirq = True
             | Just (0, Just _) <- req = not m1a
             | Just (4, Just x) <- req = bitCoerce $ lsb x
             | otherwise = m1a

        sirq' | fromStrobe vecfetch = m1a
              | fromStrobe enirq && sirq = False
              | Just (5, Just x) <- req = bitCoerce $ lsb x
              | otherwise = sirq

        activeMap | not m1a || fromStrobe vecfetch = map0
                  | otherwise = map1

    fR s = zeroExtend (pack (sSwitchedByIRQ s, sMap1Active s)) :>
           zeroExtend (sMapPtr s) :>
           zeroExtend (sMap0 s !! sMapPtr s) :>
           zeroExtend (sMap1 s !! sMapPtr s) :>
           zeroExtend (pack (sMap1Active s)) :>
           zeroExtend (pack (sSwitchedByIRQ s)) :>
           repeat (errorX "undefined MMU register")

data S v p = S
  { sMap0 :: Vec (2^v) (BitVector p)
  , sMap1 :: Vec (2^v) (BitVector p)
  , sMapPtr :: BitVector v
  , sMap1Active :: Bool
  , sSwitchedByIRQ :: Bool
  } deriving (Generic, NFDataX)

instance (KnownNat v, KnownNat p) => Default (S v p) where
  def = S
    { sMap0 = map fromIntegral indicesI
    , sMap1 = map fromIntegral indicesI
    , sMapPtr = def
    , sMap1Active = False
    , sSwitchedByIRQ = False
    }

mmuMapper :: forall v p n d x.  (KnownNat n, KnownNat v)
          => Signal d (Vec (2^v) (BitVector p))
            -- ^ Current mapping table
          -> Signal d (Maybe (BitVector (v+n), x))
            -- ^ Request from CPU in virtual space
          -> Signal d (Maybe (BitVector (p+n), x))
            -- ^ Translated request in physical space.
mmuMapper = liftA2 mmapper
  where
    mapper :: Vec (2^v) (BitVector p)
           -> BitVector (v+n)
           -> BitVector (p+n)
    mapper m a = let (top, bot) = split @_ @v @n a
                 in m !! top ++# bot
    mmapper m a = first (mapper m) <$> a
