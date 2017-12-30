{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Serial receive and transmit
module RTL.UART where

import Clash.Prelude
import Data.Maybe (isNothing)
import Control.Arrow (second)

import CFM.Types
import RTL.IOBus


uart :: (HasClockReset d g s)
     => Signal d (Maybe (BitVector 2, Maybe Cell))
     -> ( Signal d Cell
        , Signal d Bool  -- ready
        , Signal d Bool  -- idle
        , Signal d Bit  -- out
        )
uart ioreq = (ioresp, txready, txidle, txout)
  where
    (ioresp, (rate, txup)) = common txready txidle ioreq
    (txready, txidle, txout) = transmit rate txup

data CS = CS
  { csStatus :: Cell
  , csCyclesPerBit :: Cell
  }
instance Default CS where def = CS def def

common :: (HasClockReset d g s)
       => Signal d Bool
       -> Signal d Bool
       -> Signal d (Maybe (BitVector 2, Maybe Cell))
       -> ( Signal d Cell
          , ( Signal d (Unsigned 16)
            , Signal d (Maybe (BitVector 8))
            )
          )
common txreadyS txidleS ioreqS = second unbundle $ mealyp commonT commonR (bundle (txreadyS, txidleS)) ioreqS
  where
    commonR s = csStatus s :> csCyclesPerBit s :> repeat 0

    commonT s (ioreq, (txready, txidle)) = (CS status' cycPerBit', (txrate, txup))
      where
        status' = 0 ++# pack txready ++# pack txidle
    
        cycPerBit' | Just (1, Just x) <- ioreq = x
                   | otherwise = csCyclesPerBit s
    
        txup | Just (2, Just x) <- ioreq = Just $ truncateB x
             | otherwise = Nothing

        txrate = unpack $ csCyclesPerBit s


data TS = TS
  { tsBitsLeft :: Unsigned 4
    -- ^ Bits remaining in frame. This will be zero during the last bit; the
    -- transmitter is only idle if tsTimer is also zero.
  , tsShift :: BitVector 9
    -- ^ Shift register containing bits to be transmitted. The LSB of this is
    -- placed on the wire.
  , tsTimer :: Unsigned 16
    -- ^ Core cycles remaining in current bit.
  }
instance Default TS where def = TS def (-1) def

type Transmit d = Signal d (Maybe (BitVector 8))  -- new data
               -> ( Signal d Bool -- register ready
                  , Signal d Bool -- transmitter idle
                  , Signal d Bit  -- output
                  )

transmit :: (HasClockReset d g s)
         => Signal d (Unsigned 16)
         -> Transmit d
transmit = transmit' def

transmit' :: (HasClockReset d g s)
          => TS
          -> Signal d (Unsigned 16)
          -> Transmit d
transmit' s0 = curry $ mooreB transmitT transmitO s0
  where
    transmitO s = let txidle = tsBitsLeft s == 0 && tsTimer s == 0
                  in (txidle, txidle, lsb $ tsShift s)

    transmitT s (cycPerBit, regWr) = case tsBitsLeft s of
      -- Decrement the timer until empty, ignoring other state.
      _  | tsTimer s /= 0 -> s { tsTimer = tsTimer s - 1 }
      -- Once the timer is empty, if there are no bits left, check for new data.
      0 -> case regWr of
        Nothing -> s
        Just x -> s { tsTimer = cycPerBit
                    , tsBitsLeft = 1 + 8 + 2
                    , tsShift = x ++# 0
                    }
      -- Otherwise, keep shifting out bits.
      _ -> s { tsBitsLeft = tsBitsLeft s - 1
             , tsTimer = cycPerBit
             , tsShift = 1 ++# slice d8 d1 (tsShift s)
             }

addHold :: (HasClockReset d g s)
        => Transmit d
        -> Transmit d
addHold tx up = ((||) <$> txempty <*> txempty', txidle, out)
  where
    (up', txempty) = mooreB holdT holdO def (up, txempty')
    (txempty', txidle, out) = tx up'

    holdO s = (s, isNothing s)
    
    holdT _ (Just x, _) = Just x    -- allow host updates any time
    holdT _ (_, True) = Nothing     -- clear register on take
    holdT s (_, _) = s              -- otherwise preserve
