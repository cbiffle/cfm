{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
-- | A simple system consisting of a core, RAM, stacks, and a parallel
-- bidirectional "host interface."
module RTL.TargetTop where

import Clash.Prelude hiding (Word, readIO, read)
import Data.Maybe (fromMaybe, isJust, isNothing)
import CFM.Types
import RTL.IOBus
import RTL.Core

host :: (HasClockReset d g s)
     => Signal d (Maybe Word)   -- ^ Host-to-target data
     -> Signal d Bool           -- ^ Host "take" signal clears the T2H buffer
     -> Signal d (Maybe (BitVector 2, Maybe Word))  -- ^ ioreq
     -> ( Signal d (Maybe Word)
        , Signal d Bool
        , Signal d Word
        )  -- ^ Target-to-host channel, H2T buffer empty, and ioresp.
host h2tS htakeS reqS = mooreB hostT hostO def (h2tS, htakeS, reqS)
  where
    hostT (h2t, t2h, readAddr) (fromHost, hostReady, req) =
      (h2t', t2h', readAddr')
      where
        h2t' | Just _ <- fromHost = fromHost  -- host updates always overwrite
             | readAddr == 1 = Nothing        -- reads clear 1 cyc later
             | otherwise = h2t                -- otherwise, preserved
        
        t2h' | Just (3, Just v) <- req = Just v -- writes always land
             | hostReady = Nothing              -- host clears otherwise
             | otherwise = t2h
    
        readAddr' | Just (a, _) <- req = a
                  | otherwise = 0  -- don't keep clearing after read
    
    hostO (h2t, t2h, readAddr) = (t2h, h2tEmpty, resp)
      where
        resp = case readAddr of
          0 -> signExtend $ pack $ isJust h2t
          1 -> fromMaybe (errorX "host FIFO empty") h2t
          2 -> signExtend $ pack $ isNothing t2h
          _ -> errorX "to-host FIFO is write-only"
    
        h2tEmpty = isNothing h2t

target :: (HasClockReset dom gated synchronous, KnownNat n)
       => Vec n Word                -- ^ RAM image
       -> Signal dom (Maybe Word)   -- ^ Host-to-target channel
       -> Signal dom Bool           -- ^ Host "take" or ready signal
       -> ( Signal dom (Maybe Word)
          , Signal dom Bool
          ) -- ^ Target-to-host channel, and H2T empty signal, respectively.
target raminit h2t htake = (t2h, h2tEmpty)
  where
    (ioreq, _) = coreWithRAM (blockRam raminit) ioresp
    (t2h, h2tEmpty, ioresp) = host h2t htake (partialDecode ioreq)

targetTop :: (KnownNat n)
          => Vec n Word                -- ^ RAM image
          -> Clock System 'Source
          -> Reset System 'Asynchronous
          -> ( Signal System (Maybe Word)
             , Signal System Bool
             )
          -> ( Signal System (Maybe Word)
             , Signal System Bool
             )
targetTop img c r = withClockReset c r $
                    uncurry $
                    target img
