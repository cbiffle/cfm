{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module RTL.VGA.Timing
  ( timing
  , TimingSigs(..)
  , vesa800x600x56
  ) where

import Clash.Prelude
import GHC.Generics
import Data.Maybe (fromMaybe)
import Control.DeepSeq
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import CFM.Types

-- | Timing machine circuit for one axis (horizontal or vertical) of a raster
-- display.
timing :: ( HasClockReset d g s
          , KnownNat nx, KnownNat n
          , (nx + n) ~ Width
          , (n + nx) ~ Width  -- siiiiiigh
          )
       => Timing n
          -- ^ Timing values at reset.
       -> Signal d Bool
          -- ^ State machine advancement gate from sub-machine.
       -> Signal d (Maybe (BitVector 2, Maybe Cell))
          -- ^ I/O request from CPU.
       -> ( Signal d Cell
          , Signal d TimingSigs
          )
timing initial gateS ioreqS = (iorespS, outS)
  where
    (iorespS, outS) =
        mooreB fT fO (initial, def, def) (ioreqS, gateS)

    fT (tim, ts, a) (ioreq, gate) = ( timingT ioreq tim
                                    , tstateT (gate, tim) ts
                                    , fromMaybe a (fst <$> ioreq)
                                    )
    fO (tim, ts, a) = (zeroExtend (tim !! a), tstateO ts)

data TimingSigs = TimingSigs
  { timsigStartOfBlank :: Bool
  , timsigEndOfBlank :: Bool
  , timsigSyncActive :: Bool
  , timsigVidActive :: Bool
  } deriving (Show)

-------------------------------------------------------------------------------
-- Representation of timing.

-- | Phases of a horizontal or vertical timing generator.
data Phase = FrontPorch -- ^ Start of blanking interval.
           | SyncPulse  -- ^ Sync pulse.
           | BackPorch  -- ^ End of blanking interval.
           | VisibleArea  -- ^ Actual pixels.
           deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

instance Arbitrary Phase where arbitrary = genericArbitrary

-- | Phases advance cyclicly. Note that since they're a 2-bit quantity, we
-- ought to be able to do this without a carry chain. Not tested.
nextPhase :: Phase -> Phase
nextPhase = toEnum . (.&. 3) . (+ 1) . fromEnum

-- | We maintain the number of sub-cycles to spend in each state (minus 1) in a
-- simple vector, indexed by the 'Phase'. The values themselves are sized by
-- 'n' (in bits).
type Timing n = Vec 4 (BitVector n)

-- | Transition function for updating the timing structure itself. This allows
-- 'Timing' to be exposed as a set of writable registers.
timingT :: (KnownNat n, Width ~ (n + nx))
        => Maybe (BitVector 2, Maybe Cell) -> Timing n -> Timing n
timingT (Just (a, Just x)) = replace a (truncateB x)
timingT _ = id

-- | Here's an example of timing values for a common 800x600 mode.
vesa800x600x56 :: (Timing 10, Timing 10)
vesa800x600x56 = ( 23 :> 71 :> 127 :> 799 :> Nil
                 , 0 :> 1 :> 21 :> 599 :> Nil
                 )

--------------------------------------------------------------------------------
-- Timing machines and sync generation.

-- | Timing machine state, either horizontal or vertical. This machine is
-- advanced only when a gate signal allows (e.g. for vertical sync, only at
-- horizontal blanking). The timing control registers are separate and can be
-- written at any time.
data TState n = TState
  { tsPhase :: Phase
    -- ^ Current phase of the state machine.
  , tsCycLeft :: BitVector n
    -- ^ Cycles remaining within current phase.
  }
  deriving (Show, Generic, NFData)

instance Default (TState n) where def = TState VisibleArea def
instance (KnownNat n) => Arbitrary (TState n) where arbitrary = genericArbitrary

-- | Transition function for updating the state of a timing machine.
--
-- The argument order is flipped from our usual Moore machines because of how
-- it gets used below.
tstateT :: (KnownNat n)
        => (Bool, Timing n) -- ^ Gate and configuration.
        -> TState n
        -> TState n
tstateT (False, _) s = s  -- maintain state when gated
tstateT (_, tm) s = TState { tsPhase = phase'
                           , tsCycLeft = cyc'
                           }
  where
    zero = tsCycLeft s == 0
    phase = tsPhase s
    next = nextPhase phase
    -- Advance the phase and reload the counter when the counter expires.
    (phase', cyc') | zero      = (next,  tm !! next)
                   | otherwise = (phase, pred (tsCycLeft s))

-- | Timing machine output function.
--
-- This produces two "event" outputs and two "level" outputs.
--
-- The event outputs are 'True' for exactly one cycle at a specific event:
--
-- - Start-of-blank is 'True' during the final cycle of the visible area.
-- - End-of-blank is 'True' during the final cycle of the blanking interval.
--
-- The level outputs are 'True' while a condition holds:
--
-- - Sync is 'True' while the sync pulse should be generated.
-- - Active is 'True' while we're in the visible area of timing.
tstateO :: (KnownNat n)
        => TState n
        -> TimingSigs
tstateO s = TimingSigs blank eblank sync active
  where
    blank = tsCycLeft s == 0 && tsPhase s == VisibleArea
    eblank = tsCycLeft s == 0 && tsPhase s == BackPorch
    sync = tsPhase s == SyncPulse
    active = tsPhase s == VisibleArea
