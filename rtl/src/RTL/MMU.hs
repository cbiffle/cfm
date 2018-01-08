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

-- TODO fix all the fromIntegral uses in this module.

mmu :: forall n v p ev ep d g s.
       ( HasClockReset d g s
       , KnownNat v
       , KnownNat p
       , KnownNat n
       , KnownNat ev
       , KnownNat ep
       , Width ~ (ev + v)
       , Width ~ (ep + p)
       )
    => SNat v
    -> SNat p
    -> SNat n
    -> Signal d (Maybe (BitVector 2, Maybe Cell))
    -> ( Signal d Cell
        , Signal d (Maybe (BitVector (v+n), Maybe Cell))
          -> Signal d (Maybe (BitVector (p+n), Maybe Cell))
        )
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

mmu' :: forall v p ev ep d g s.
        ( HasClockReset d g s
        , KnownNat v
        , KnownNat p
        , KnownNat ev
        , KnownNat ep
        , Width ~ (ev + v)
        , Width ~ (ep + p)
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

        mp' | Just (1, Just v) <- req = fromIntegral v
            | Just (a, Just _) <- req, a == 2 || a == 3 = mp + 1
            | otherwise = mp

        map0' | Just (2, Just v) <- req = replace mp (fromIntegral v) map0
              | otherwise = map0

        map1' | Just (3, Just v) <- req = replace mp (fromIntegral v) map1
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
  -- | The default mapping is the identity mapping for the bottom 32kiB.
  -- At reset, map 0 is active.
  def = S
    { sMap0 = repeat 0
    , sMap1 = repeat 0
    , sSel = False
    , sMapPtr = def
    }
