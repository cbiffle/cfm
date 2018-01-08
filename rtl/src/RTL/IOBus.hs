{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IO bus support and interfacing
module RTL.IOBus where

import Clash.Prelude hiding (read)

import Control.Monad (join)
import Control.Arrow (first)
import Data.Maybe (fromMaybe, isJust)
import CFM.Types

-- | Addresses with the top bit set are I/O addresses, leaving 14 bits of space
-- for I/O devices.
type IOAddr = BitVector 14

-- | Bridges the core's outgoing bus to the narrower I/O bus
coreToIO :: Signal d CellAddr                       -- ^ read address from core
         -> Signal d (Maybe (CellAddr, t))          -- ^ write command from core
         -> ( Signal d (Maybe (IOAddr, Maybe t))    -- ^ combined I/O interface
            , Signal d Bool                         -- ^ I/O write active signal
            )
coreToIO read write = unbundle $ datapath <$> read <*> write
  where
    datapath _ (Just (split -> (1, wa), wd)) = (Just (wa, Just wd), True)
    datapath (split -> (1, ra)) _ = (Just (ra, Nothing), False)
    datapath _ _ = (Nothing, False)


-- | Splits an I/O command from the core into device-specific commands, and
-- provides information for the response multiplexer.
--
-- Specifically, this splits an @m + n@ bit address into a device selector (the
-- top @m@ bits) and a device address (the bottom @n@ bits). The I/O command is
-- then demultiplexed onto @2 ^ m@ different channels. The index of the channel
-- chosen is separately provided to be latched by the response mux.
ioDecoder :: forall m n t d. (KnownNat m, KnownNat n, CmpNat m 0 ~ 'GT)
          => Signal d (Maybe (BitVector (m + n), Maybe t))
          -> ( Vec (2 ^ m) (Signal d (Maybe (BitVector n, Maybe t)))
             , Signal d (Maybe (BitVector m))
             )
ioDecoder = first unbundle . unbundle . fmap ioDecoder'

ioDecoder' :: forall m n t. (KnownNat m, KnownNat n, CmpNat m 0 ~ 'GT)
           => Maybe (BitVector (m + n), Maybe t)
           -> (Vec (2 ^ m) (Maybe (BitVector n, Maybe t)), Maybe (BitVector m))
ioDecoder' input = (map (\i -> join (gate i <$> top <*> input')) indicesI, ch)
  where
    gate :: Index (2 ^ m) -> BitVector m -> a -> Maybe a
    gate i b | pack i == b = Just
             | otherwise = const Nothing
    input' = truncateAddr input
    top = topBits . fst <$> input
    ch = join (maybe top (const Nothing) . snd <$> input)

truncateAddr :: (KnownNat n)
             => Maybe (BitVector (m + n), t)
             -> Maybe (BitVector n, t)
truncateAddr = fmap $ first truncateB

topBits
  :: (KnownNat bits, KnownNat extra)
  => BitVector (bits + extra)
  -> BitVector bits
topBits = fst . split

-- | Routes device responses.
--
-- Each cycle, the 'ioDecoder' sends its muxing decision as a @BitVector m@
-- (when an I/O device is selected at all. On the next cycle, the 'responseMux'
-- selects the corresponding channel out of @2 ^ m@ device response channels.
responseMux :: forall m t d g s. (KnownNat m, HasClockReset d g s)
            => Vec (2 ^ m) (Signal d t)  -- ^ response from each device
            -> Signal d (Maybe (BitVector m)) -- ^ decoder output
            -> Signal d t  -- ^ response to core
responseMux inputs ch' = (!!) <$> bundle inputs
                              <*> regEn 0 (isJust <$> ch')
                                          (fromMaybe undefined <$> ch')

-- | Applies a partial decode of an @m + n@ bit IO request down to an @n@ bit
-- request. This has the effect of making the device that consumes the @n@ bit
-- request appear repeated @2^m@ times through the address space.
partialDecode :: (KnownNat n)
              => Signal d (Maybe (BitVector (m + n), Maybe t))
              -> Signal d (Maybe (BitVector n, Maybe t))
partialDecode = fmap truncateAddr


-- | Implementation strategy for a peripheral device that can be described as a
-- Moore machine. This is like Clash's 'moore' but provides a bus read response
-- multiplexer and associated state.
--
-- The reset state of the machine is given by 'def' for the state type, for
-- convenience.
moorep :: (KnownNat a, HasClockReset dom gated synchronous, Default s)
       => (s -> (Maybe (BitVector a, Maybe Cell), i) -> s)
        -- ^ State transition function.
       -> (s -> Vec (2^a) Cell)
        -- ^ Readable register projection function.
       -> (s -> o)
        -- ^ Output projection function.
       -> Signal dom i
        -- ^ Peripheral-specific input signals.
       -> Signal dom (Maybe (BitVector a, Maybe Cell))
        -- ^ Request from I/O bus.
       -> ( Signal dom Cell
          , Signal dom o
          )     -- ^ Response to I/O bus and peripheral-specific outputs.
moorep ft fr fo = \inp ioreq ->
  let (ioresp, outp) = unbundle $
                       moore ft' fo' def $
                       bundle (ioreq, inp)
      ft' (s, a) (ioreq_, i) = (ft s (ioreq_, i), fromMaybe a (fst <$> ioreq_))
      fo' (s, a) = (fr s !! a, fo s)
  in (ioresp, outp)
{-# INLINE moorep #-}

-- | Implementation strategy for a peripheral device that can be described as a
-- Mealy machine. This is like Clash's 'mealy' but provides a bus read response
-- multiplexer and associated state.
--
-- Note that 'moorep' will almost always be faster.
--
-- The reset state of the machine is given by 'def' for the state type, for
-- convenience.
mealyp :: (KnownNat a, HasClockReset dom gated synchronous, Default s)
       => (s -> (Maybe (BitVector a, Maybe Cell), i) -> (s, o))
        -- ^ State transition and outputs function.
       -> (s -> Vec (2^a) Cell)
        -- ^ Readable register projection function.
       -> Signal dom i
        -- ^ Peripheral-specific input signals.
       -> Signal dom (Maybe (BitVector a, Maybe Cell))
        -- ^ Request from I/O bus.
       -> ( Signal dom Cell
          , Signal dom o
          )     -- ^ Response to I/O bus and peripheral-specific outputs.
mealyp ft fr =
  \inp ioreq ->
    let (ioresp, outp) = unbundle $
                         mealy ft' def $
                         bundle (ioreq, inp)
        ft' (s, a) (ioreq_, i) = let (s', o) = ft s (ioreq_, i)
                                 in ( (s', fromMaybe a (fst <$> ioreq_))
                                    , (fr s !! a, o)
                                    )
    in (ioresp, outp)
{-# INLINE mealyp #-}
