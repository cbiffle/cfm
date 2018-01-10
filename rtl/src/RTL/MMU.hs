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
    -> Signal d (Maybe (BitVector 2, Maybe Cell))
      -- ^ Connection to the I/O bus.
    -> ( Signal d Cell
        , Signal d (Maybe (BitVector (v+n), Maybe Cell))
          -> Signal d (Maybe (BitVector (p+n), Maybe Cell))
        ) -- ^ I/O response and mapping constructor, respectively.
mmu _ _ _ ioreq = (ioresp, liftA2 mmapper cmap)
  where
    (ioresp, cmap) = mmu' @v @p @ev @ep ioreq

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
     => Signal d (Maybe (BitVector 2, Maybe Cell))
     -> ( Signal d Cell
        , Signal d (Vec (2^v) (BitVector p))
        )
mmu' = moorep fT fR fO (pure ())
  where
    fT :: S v p -> (Maybe (BitVector 2, Maybe Cell), ()) -> S v p
    fT (S map0 map1 sel mp) (req, _) = S map0' map1' sel' mp'
      where
        sel' | Just (0, Just v) <- req = v .&. 1 /= 0
             | otherwise = sel

        mp' | Just (1, Just v) <- req = truncateB v
            | Just (a, Just _) <- req, a == 2 || a == 3 = mp + 1
            | otherwise = mp

        map0' | Just (2, Just v) <- req = replace mp (truncateB v) map0
              | otherwise = map0

        map1' | Just (3, Just v) <- req = replace mp (truncateB v) map1
              | otherwise = map1

    fR s = zeroExtend (pack (sSel s)) :>
           zeroExtend (sMapPtr s) :>
           zeroExtend (sMap0 s !! sMapPtr s) :>
           zeroExtend (sMap1 s !! sMapPtr s) :>
           Nil
    fO s | sSel s = sMap1 s
         | otherwise = sMap0 s

data S v p = S
  { sMap0 :: Vec (2^v) (BitVector p)
  , sMap1 :: Vec (2^v) (BitVector p)
  , sSel :: Bool
  , sMapPtr :: BitVector v
  }

instance (KnownNat v, KnownNat p) => Default (S v p) where
  -- | At reset, physical page 0 appears in every position, because it
  -- simplifies the hardware.
  def = S
    { sMap0 = repeat 0
    , sMap1 = repeat 0
    , sSel = False
    , sMapPtr = def
    }
