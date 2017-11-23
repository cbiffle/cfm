{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | A stack designed to use flops in shift-register configuration, instead of
-- inferring a BlockRAM. This might be useful since we've got more flops than
-- RAMs.
module FlopStack where

import Clash.Prelude hiding (Word)
import Data.Maybe (fromMaybe)

import Types

flopStack :: ( KnownNat depth
             , HasClockReset d g s
             )
          => SNat (depth + 1)
          -> Signal d (Signed 2)    -- ^ delta
          -> Signal d (Maybe Word)  -- ^ write signal
          -> Signal d Word          -- ^ read output
flopStack dnat = flopStack' $ replicate dnat $ errorX "undefined"

flopStack' :: ( KnownNat depth
              , HasClockReset d g s
              )
           => Vec (depth + 1) Word   -- ^ initial contents for test
           -> Signal d (Signed 2)    -- ^ delta
           -> Signal d (Maybe Word)  -- ^ write signal
           -> Signal d Word          -- ^ read output
flopStack' contents delta write = head registers
  where
    registers = zipWith register
                        contents
                        ((fromMaybe <$> head updates <*> write) :> tail updates)

    delta1 = unpack . slice d1 d1 <$> delta
    delta0 = unpack . slice d0 d0 <$> delta
    updates = zipWith (mux delta1)
                      shiftedLeft1 -- 1x
                      (zipWith (mux delta0) shiftedRight1 registers)   -- 0x

    shiftedLeft1 = tail registers :< pure (errorX "underflow")
    --shiftedLeft2 = tail shiftedLeft1 :< pure (errorX "underflow")
    shiftedRight1 = pure (errorX "undefined") :> init registers
