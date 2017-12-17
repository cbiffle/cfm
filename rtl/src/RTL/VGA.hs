{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module RTL.VGA where

import Clash.Prelude
import Control.Lens hiding ((:>))
import CFM.Types

-------------------------------------------------------------------------------
-- Timing and sync generation

-- | Timing machine state, either horizontal or vertical. This contains the
-- state with gated updates; the timing control registers, which can be written
-- at any time, are external.
data TState n = TState
  { _tsPhase :: Phase
    -- ^ Current phase of the state machine.
  , _tsCycLeft :: BitVector n
    -- ^ Cycles remaining within current phase.
  }

data Phase = FrontPorch
           | SyncPulse
           | BackPorch
           | VisibleArea
           deriving (Show, Eq, Ord, Enum, Bounded)

makeLenses ''TState

-- | Phases advance cyclicly. Note that since they're a 2-bit quantity, we
-- ought to be able to do this without a carry chain. Not tested.
nextPhase :: Phase -> Phase
nextPhase = toEnum . (.&. 3) . (+ 1) . fromEnum

instance Default (TState n) where def = TState VisibleArea def

-- | We maintain the number of sub-cycles to spend in each state in a simple
-- vector, indexed by the 'Phase'. The values themselves are sized by 'n'.
type Timing n = Vec 4 (BitVector n)

vesa800x600x60 :: (Timing 10, Timing 10)
vesa800x600x60 = ( 39 :> 127 :> 87 :> 799 :> Nil
                 , 0 :> 3 :> 22 :> 599 :> Nil
                 )

timingT :: Maybe (BitVector 2, Maybe (BitVector n)) -> Timing n -> Timing n
timingT (Just (a, Just x)) = replace a x
timingT _ = id

-- | Transition function for updating the state of a timing machine.
tstateT :: (KnownNat n)
        => TState n
        -> (Bool, Timing n)
        -> TState n
tstateT s (False, _) = s  -- maintain state when gated
tstateT s (_, tm) = s & tsPhase .~ phase'
                      & tsCycLeft .~ cyc'
  where
    zero = s ^. tsCycLeft == 0
    phase = s ^. tsPhase
    next = nextPhase phase
    phase' | zero = next
           | otherwise = phase
    cyc' | zero = tm !! next
         | otherwise = pred (s ^. tsCycLeft)

-- | Timing machine output function.
--
-- The blanking output signal can be used to produce interrupts, and also to
-- gate the vertical machine from the horizontal.
tstateO :: (KnownNat n)
        => TState n
        -> (Bool, Bool, Bool)
tstateO s = (blank, sync, active)
  where
    blank = s ^. tsCycLeft == 0 && s ^.tsPhase == VisibleArea
    sync = s ^. tsPhase == SyncPulse
    active = s ^. tsPhase == VisibleArea


-------------------------------------------------------------------------------
-- CRTC frame control and pixel addressing

data GState = GState
  { _gsH :: (TState 10, Timing 10)
    -- ^ Horizontal timing configuration and state.
  , _gsV :: (TState 10, Timing 10)
    -- ^ Vertical timing configuration and state.
  , _gsPixels :: BitVector 14
    -- ^ Pixel addressing counter.
  , _gsHIF :: Bool
    -- ^ Hblank Interrupt Flag
  , _gsVIF :: Bool
    -- ^ Vblank Interrupt Flag
  , _gsFB :: BitVector 4
    -- ^ Font Base
  }

makeLenses ''GState

instance Default GState where
  def = GState (def, fst vesa800x600x60)
               (def, snd vesa800x600x60)
               def False False def

-- | Mealy function for the framegen circuit.
--
-- This is responsible for applying updates from the I/O bus to the state, and
-- generating the outputs. (Phrasing this as a Moore machine was duplicative.)
framegenT :: GState
          -> Maybe (BitVector 4, Maybe Cell)
          -> ( GState
             , ( Bool -- hsync
               , Bool -- vsync
               , Bool -- HIF
               , Bool -- VIF
               , BitVector 14 -- pixel address
               , BitVector 4  -- font base
               ))
framegenT s iowr = (s', (hsync, vsync, s ^. gsHIF, s ^. gsVIF, vid, s ^. gsFB))
  where
    s' = s & gsH . _1 %~ flip tstateT (True, s ^. gsH . _2)
           & gsH . _2 %~ timingT hwr
           & gsV . _1 %~ flip tstateT (hblank, s ^. gsV . _2)
           & gsV . _2 %~ timingT vwr
           & gsPixels .~ pixels'
           & gsHIF %~ ((&& not hack) . (|| hblank))
           & gsVIF %~ ((&& not vack) . (|| vblank))
           & gsFB .~ fb'

    iosplit (Just (split -> (t, a), x)) = case t of
      0 -> (Just (a, truncateB <$> x), Nothing, Nothing)
      1 -> (Nothing, Just (a, truncateB <$> x), Nothing)
      _ -> (Nothing, Nothing, Just (a, truncateB <$> x))
    iosplit _ = (Nothing, Nothing, Nothing)

    (hwr, vwr, rwr) = iosplit iowr

    pixels' | Just (0, Just v) <- rwr = v
            | hactive && vactive = s ^. gsPixels + 1
            | otherwise = s ^. gsPixels

    (hack, vack) | Just (1, Just v) <- rwr = unpack (slice d1 d0 v)
                 | otherwise = (False, False)

    fb' | Just (2, Just v) <- rwr = truncateB v
        | otherwise = s ^. gsFB

    (hblank, hsync, hactive) = tstateO (s ^. gsH . _1)
    (vblank, vsync, vactive) = tstateO (s ^. gsV . _1)
    vid = if hactive && vactive
            then s ^. gsPixels
            else 0

framegen :: (HasClockReset d g s)
         => Signal d (Maybe (BitVector 4, Maybe Cell))
         -> ( Signal d Bool   -- hsync
            , Signal d Bool   -- vsync
            , Signal d Bool   -- hblank interrupt
            , Signal d Bool   -- vblank interrupt
            , Signal d (BitVector 14) -- pixel address
            , Signal d (BitVector 4)  -- glyph base
            )
framegen = unbundle . mealy framegenT def
