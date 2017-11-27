{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | IO bus support and interfacing
module IOBus where

import Clash.Prelude hiding (Word)

import Types

-- | Addresses with the top bit set are I/O addresses, leaving 14 bits of space
-- for I/O devices.
type IOAddr = BitVector 14

-- | Bridges the core's outgoing bus to the narrower I/O bus
coreToIO :: Signal d WordAddr   -- ^ read address from core
         -> Signal d (Maybe (WordAddr, Word)) -- ^ write command from core
         -> Signal d (Maybe (IOAddr, Maybe Word))  -- ^ combined I/O interface
coreToIO = liftA2 datapath
  where
    datapath _ (Just (split -> (1, wa), wd)) = Just (wa, Just wd)
    datapath (split -> (1, ra)) _ = Just (ra, Nothing)
    datapath _ _ = Nothing


