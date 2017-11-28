{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IO bus support and interfacing
module IOBus where

import Clash.Prelude hiding (Word, read)

import Control.Monad (join)
import Control.Arrow (first)
import Types

-- | Addresses with the top bit set are I/O addresses, leaving 14 bits of space
-- for I/O devices.
type IOAddr = BitVector 14

-- | Bridges the core's outgoing bus to the narrower I/O bus
coreToIO :: Signal d WordAddr                       -- ^ read address from core
         -> Signal d (Maybe (WordAddr, t))          -- ^ write command from core
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
            -> Signal d (Maybe t)  -- ^ response to core
responseMux inputs ch' = switch <$> bundle inputs <*> ch
  where
    ch = register Nothing ch'
    switch is = fmap (is !!)
