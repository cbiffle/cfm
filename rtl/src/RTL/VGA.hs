{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module RTL.VGA where

import Clash.Prelude

-- Horizontal and vertical timing are very similar, albeit at different rates.
-- Each consists of four phases:
-- - A sync pulse.
-- - The "back porch" idle area.
-- - Active video.
-- - The "front porch" idle area.
--
-- Together, the porches and the sync pulse determine the blanking interval.
-- We can model the overall display timing as a set of two nested state
-- machines. The horizontal timing machine is the faster of the two, driven at
-- the pixel clock. The vertical timing machine is slower, and is advanced by
-- the horizontal machine at the start of its blanking interval. (The precise
-- point where the horizontal timing advances the vertical is vague in the
-- standard, and monitors try to adjust to variations there. I've chosen the
-- start of blanking because it's also a useful moment for generating an
-- interrupt.)

-- | The state machines go through this cycle.
data ScanState = FrontPorch
               | SyncPulse
               | BackPorch
               | VisibleArea
               deriving (Show, Eq, Ord, Enum, Bounded)

-- | States advance cyclicly. Note that since they're a 2-bit quantity, we ought
-- to be able to do this without a carry chain. Not tested.
nextState :: ScanState -> ScanState
nextState = toEnum . (.&. 3) . (+ 1) . fromEnum

-- | We maintain the number of sub-cycles to spend in each state in a simple
-- vector, indexed by the 'ScanState'. The values themselves are sized by 'n'.
type Timing n = Vec 4 (BitVector n)

-- | State machine driver function. Produces the next state, next counter value,
-- and a transition signal that can be used to gate a higher-level state machine
-- or produce interrupts.
advance :: (KnownNat n)
        => Timing n     -- ^ configuration
        -> ScanState    -- ^ current state
        -> BitVector n  -- ^ counter value
        -> Bool         -- ^ gating
        -> (ScanState, BitVector n, Bool)
advance _   s                 c False = (s, c, False)
advance cfg (nextState -> s') 0 _     = (s', cfg !! s', True)
advance _   s                 c _     = (s, pred c, False)

-- | Generates the sync signal from the state. Note that the actual sync output
-- needs to be inverted for some modes.
sync :: ScanState -> Bool
sync = (== SyncPulse)

active :: ScanState -> Bool
active = (== VisibleArea)

data S = S
  { sTiming :: Timing 10
  , sCtr :: BitVector 10
  , sState :: ScanState
  } deriving (Show)

syncgen :: (HasClockReset d g s)
        => Timing 10
        -> Signal d Bool
        -> ( Signal d Bool
           , Signal d Bool
           , Signal d Bool
           , Signal d (BitVector 6)
           )
syncgen tm = unbundle . mealy syncgenT (S tm 0 FrontPorch)
  where
    syncgenT :: S -> Bool -> (S, (Bool, Bool, Bool, BitVector 6))
    syncgenT s g =
      let (state', ctr', tx) = advance (sTiming s) (sState s) (sCtr s) g
      in ( S { sTiming = sTiming s
             , sCtr = ctr'
             , sState = state'
             }
         , (tx, sync (sState s), active (sState s), truncateB (sCtr s))
         )

framegen :: (HasClockReset d g s)
         => (Timing 10, Timing 10)
         -> Signal d (Bool, Bool, BitVector 6)
framegen (htm, vtm) = bundle (hsync, vsync, vid)
  where
    (htx, hsync, hact, hout) = syncgen htm (pure True)
    (_, vsync, vact, vout) = syncgen vtm ((&&) <$> htx <*> hsync)
    vid = mux ((&&) <$> hact <*> vact)
              (xor <$> hout <*> vout)
              (pure 0)

vesa800x600x60 :: (Timing 10, Timing 10)
vesa800x600x60 = ( 39 :> 127 :> 87 :> 799 :> Nil
                 , 0 :> 3 :> 22 :> 599 :> Nil
                 )
