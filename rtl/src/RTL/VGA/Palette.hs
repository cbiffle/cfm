{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RTL.VGA.Palette where

import Clash.Prelude
import CFM.Types
import RTL.IOBus (moorep)

-- | The palette maps @2^n@ attribute colors to output colors of @c@ bits each.
-- It is a small asynchronous memory with N read ports (e.g. foreground and
-- background), and a secondary synchronous port for the CPU.
palette :: forall n c rp cx d g s.
           ( HasClockReset d g s
           , KnownNat n
           , KnownNat c
           , KnownNat cx
           , Width ~ (cx + c)
           , Width ~ (c + cx) -- siiiiigh nat normalize
           )
        => Vec rp (Signal d (BitVector n))
          -- ^ Asynchronous read ports for the video hardware
        -> Signal d (Maybe (BitVector n, Maybe Cell))
          -- ^ I/O request from CPU (synchronous)
        -> ( Vec rp (Signal d (BitVector c))
           , Signal d Cell
           ) -- ^ Read response to video hardware and CPU, respectively.
palette ards ioreq = (map (liftA2 (!!) array) ards, ioresp)
  where
    (ioresp, array) = moorep fT (map zeroExtend) id (pure ()) ioreq

    -- This type signature is the only place the vector's size is fixed,
    -- because Vec accessors take Enum. Don't try to elide it.
    fT :: Vec (2^n) (BitVector c)
       -> (Maybe (BitVector n, Maybe Cell), ())
       -> Vec (2^n) (BitVector c)
    fT vec (Just (a, Just v), _) = replace a (truncateB v) vec
    fT vec _ = vec
