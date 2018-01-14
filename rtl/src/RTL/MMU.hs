{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Memory Management Unit for accessing more than 32kiB of RAM.
module RTL.MMU where

import Clash.Prelude
import Control.Arrow (first)

import CFM.Types
import RTL.IOBus

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
-- page translation table. It returns a function, which acts as a constructor
-- for instantiating address mapping hardware. This constructor could be used
-- more than once, if you wanted to translate multiple independent addressing
-- paths using the same translation tables. Each time the constructor is applied
-- costs one read mux on the translation table.
--
-- The MMU contains two sets of mapping registers, map 0 and map 1. At reset,
-- map 0 is active. The map can be switched by writing the Control/Status
-- register with any value. This is intended to be fused into a return
-- instruction, to effect an atomic jump-and-change-maps operation.
--
-- Interrupt entry causes an automatic switch back to map 0 at the start of
-- vector fetch, so that the vector is always fetched from map 0. When this
-- occurs, bit 1 in the Control/Status register will be set. This bit is cleared
-- by switching maps.
--
-- Register set:
--
-- - +0: Control/Status. Any write toggles bit 0 and clears bit 1.
--   - Bit 0: active map.
--   - Bit 1: switched due to IRQ flag.
-- - +2: Map Pointer. Holds a 'v'-bit index of a map register.
-- - +4: Map 0 Access. Reads/writes the part of map 0 selected by Map Pointer.
-- - +6: Map 1 Access. Reads/writes the part of map 1 selected by Map Pointer.
mmu :: forall n v p ev ep d g s.
       ( HasClockReset d g s
       , KnownNat v
       , KnownNat p
       , KnownNat n
       , KnownNat ev
       , KnownNat ep
       , (v + ev) ~ Width
       , (ev + v) ~ Width  -- seriously, NatNormalise does not obviate this
       , (p + ep) ~ Width
       , (ep + p) ~ Width
       )
    => SNat v
      -- ^ Number of translated virtual address bits.
    -> SNat p
      -- ^ Number of physical bits produced after translation.
    -> SNat n
      -- ^ Number of untranslated bits. A page is @2^n@ words in size.
    -> Signal d Bool
      -- ^ Interrupt signal from controller.
    -> Signal d (Maybe (BitVector 2, Maybe Cell))
      -- ^ Connection to the I/O bus.
    -> ( Signal d Cell
        , Signal d (Maybe (BitVector (v+n), Maybe Cell))
          -> Signal d (Maybe (BitVector (p+n), Maybe Cell))
        ) -- ^ I/O response and mapping constructor, respectively.
mmu _ _ _ irq ioreq = (ioresp, liftA2 mmapper cmap)
  where
    (ioresp, cmap) = mmu' @v @p @ev @ep irq ioreq

    mapper :: Vec (2^v) (BitVector p)
           -> BitVector (v+n)
           -> BitVector (p+n)
    mapper m a = let (top, bot) = split @_ @v @n a
                 in m !! top ++# bot
    mmapper :: Vec (2^v) (BitVector p)
            -> Maybe (BitVector (v+n), x)
            -> Maybe (BitVector (p+n), x)
    mmapper m a = first (mapper m) <$> a

-- | Implementation factor of 'mmu' that gives access to the raw translation
-- table, instead of providing a mapping constructor.
mmu' :: forall v p ev ep d g s.
        ( HasClockReset d g s
        , KnownNat v
        , KnownNat p
        , KnownNat ev
        , KnownNat ep
        , (ev + v) ~ Width
        , (v + ev) ~ Width
        , (ep + p) ~ Width
        , (p + ep) ~ Width
        )
     => Signal d Bool
     -> Signal d (Maybe (BitVector 2, Maybe Cell))
     -> ( Signal d Cell
        , Signal d (Vec (2^v) (BitVector p))
        )
mmu' = mealyp fT fR
  where
    fT :: S v p
       -> (Maybe (BitVector 2, Maybe Cell), Bool)
       -> (S v p, Vec (2^v) (BitVector p))
    fT (S map0 map1 mp m1a sirq) (req, irq) = (s', activeMap)
      where
        s' = S map0' map1' mp' m1a' sirq'

        mp' | Just (1, Just v) <- req = truncateB v
            | otherwise = mp

        map0' | Just (2, Just v) <- req = replace mp (truncateB v) map0
              | otherwise = map0

        map1' | Just (3, Just v) <- req = replace mp (truncateB v) map1
              | otherwise = map1

        m1a' | Just (0, Just _) <- req = not m1a
             | otherwise = m1a

        sirq' | irq = m1a
              | Just (0, Just _) <- req = False
              | otherwise = sirq

        activeMap | not m1a || irq = map0
                  | otherwise = map1

    fR s = zeroExtend (pack (sSwitchedByIRQ s, sMap1Active s)) :>
           zeroExtend (sMapPtr s) :>
           zeroExtend (sMap0 s !! sMapPtr s) :>
           zeroExtend (sMap1 s !! sMapPtr s) :>
           Nil

data S v p = S
  { sMap0 :: Vec (2^v) (BitVector p)
  , sMap1 :: Vec (2^v) (BitVector p)
  , sMapPtr :: BitVector v
  , sMap1Active :: Bool
  , sSwitchedByIRQ :: Bool
  }

instance (KnownNat v, KnownNat p) => Default (S v p) where
  def = S
    { sMap0 = map fromIntegral indicesI
    , sMap1 = map fromIntegral indicesI
    , sMapPtr = def
    , sMap1Active = False
    , sSwitchedByIRQ = False
    }
