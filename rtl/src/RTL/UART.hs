{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Serial receive and transmit
module RTL.UART where

import Clash.Prelude
import Data.Maybe (isNothing, isJust)
import Control.Arrow (second)

import CFM.Types
import RTL.IOBus


-- | A simple UART.
--
-- This currently provides one level of transmit queueing.
uart :: (HasClockReset d g s)
     => Signal d Bit
     -> Signal d (Maybe (BitVector 2, Maybe Cell))
     -> ( Signal d Cell
        , Signal d Bool  -- ready
        , Signal d Bool  -- idle
        , Signal d Bool  -- RXNE
        , Signal d Bit  -- out
        )
uart din ioreq = (ioresp, txready, txidle, rxne, txout)
  where
    (ioresp, (rate, txup, rxne)) = common txready txidle rx ioreq
    (txready, txidle, txout) = addTxHold (transmit rate) txup
    rx = receive rate din


------------------------------------------------------------------------------
-- Common registers and interface for both the transmitter and receiver.

common :: (HasClockReset d g s)
       => Signal d Bool -- ^ transmit register empty / ready
       -> Signal d Bool -- ^ transmitter idle
       -> Signal d (Maybe (Either FramingError (BitVector 8)))
       -> Signal d (Maybe (BitVector 2, Maybe Cell))  -- ^ io req
       -> ( Signal d Cell
          , ( Signal d (Unsigned 16)
            , Signal d (Maybe (BitVector 8))
            , Signal d Bool
            )
          ) -- ^ ioresp, rate divisor, TX register update, rxne
common txreadyS txidleS rxS ioreqS = second unbundle $
                                     mealyp commonT commonR
                                     (bundle (txreadyS, txidleS, rxS))
                                     ioreqS
  where
    commonR s = csStatus s :>
                csCyclesPerBit s :>
                0 :>
                fmt (csReceived s) :>
                Nil
      where
        fmt Nothing = 0
        fmt (Just (Left _)) = -1
        fmt (Just (Right b)) = zeroExtend b

    commonT s (ioreq, (txready, txidle, rx)) =
      (CS status' cycPerBit' rx' dt', (txrate, txup, rxne))
      where
        status' = 0 ++# pack rxne ++# pack txready ++# pack txidle

        rxne = isJust $ csReceived s
    
        cycPerBit' | Just (1, Just x) <- ioreq = x
                   | otherwise = csCyclesPerBit s
    
        txup | Just (2, Just x) <- ioreq = Just $ truncateB x
             | otherwise = Nothing

        rx' | csDelayedTake s = Nothing
            | Just v <- rx = Just v
            | otherwise = csReceived s

        dt' | Just (3, Nothing) <- ioreq = True
            | otherwise = False

        txrate = unpack $ csCyclesPerBit s

data CS = CS
  { csStatus :: Cell
  , csCyclesPerBit :: Cell
  , csReceived :: Maybe (Either FramingError (BitVector 8))
  , csDelayedTake :: Bool
  }
instance Default CS where def = CS def def def False


------------------------------------------------------------------------------
-- Transmitter.

-- | Abstract type of transmitters in clock domain 'd'.
type Transmit d = Signal d (Maybe (BitVector 8))  -- new data
               -> ( Signal d Bool -- register ready
                  , Signal d Bool -- transmitter idle
                  , Signal d Bit  -- output
                  )

-- | Transmitter with a single shift register. A new value cannot be loaded for
-- transmission until the transmitter is completely idle (that is, ready ==
-- idle).
transmit :: (HasClockReset d g s)
         => Signal d (Unsigned 16)
         -> Transmit d
transmit = curry $ mooreB transmitT transmitO def
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

-- | State for the simple transmitter.
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

-- | Adds a single level of transmit holding buffer to a transmitter. This
-- causes TX ready and TX idle to be decoupled by one stage, and also delays the
-- initiation of transmission by one core clock cycle.
--
-- This can be stacked if desired.
addTxHold :: (HasClockReset d g s)
          => Transmit d
          -> Transmit d
addTxHold tx up = ((||) <$> txempty <*> txempty', txidle, out)
  where
    (up', txempty) = mooreB holdT holdO def (up, txempty')
    (txempty', txidle, out) = tx up'

    holdO s = (s, isNothing s)
    
    holdT _ (Just x, _) = Just x    -- allow host updates any time
    holdT _ (_, True) = Nothing     -- clear register on take
    holdT s (_, _) = s              -- otherwise preserve


------------------------------------------------------------------------------
-- Receiver.

receive :: (HasClockReset d g s)
        => Signal d (Unsigned 16)
        -> Signal d Bit
        -> Signal d (Maybe (Either FramingError (BitVector 8)))
receive = curry $ mooreB receiveT receiveO def

data RS = RS
  { rsBitsLeft :: Unsigned 4
  , rsShift :: BitVector 10
  , rsTimer :: Unsigned 16
  , rsNew :: Bool
  }
instance Default RS where def = RS def def def False

receiveT :: RS -> (Unsigned 16, Bit) -> RS
receiveT s0 (cycPerBit, b) =
  let s = s0 { rsNew = False
             }
  in case rsBitsLeft s of
    _ | rsTimer s /= 0 -> s { rsTimer = rsTimer s - 1 }
  
    0 -> if b == 0
            then s { rsBitsLeft = 10
                   , rsTimer = cycPerBit `shiftR` 1 -- first sample after 1/2
                   }
            else s
    _ -> s { rsBitsLeft = rsBitsLeft s - 1
           , rsTimer = if rsBitsLeft s == 1 then 0 else cycPerBit
           , rsShift = b ++# slice d9 d1 (rsShift s)
           , rsNew = rsBitsLeft s == 1
           }

data FramingError = FramingError

receiveO :: RS -> Maybe (Either FramingError (BitVector 8))
receiveO s
  | rsNew s = 
      if slice d9 d9 (rsShift s) == 1 && slice d0 d0 (rsShift s) == 0
         then Just $ Right $ slice d8 d1 $ rsShift s
         else Just $ Left FramingError
  | otherwise = Nothing
