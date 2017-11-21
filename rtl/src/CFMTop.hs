{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module CFMTop where

import Clash.Prelude hiding (Word)
import Control.Lens
import Data.Tuple (swap)
import Str
import Types

core :: HasClockReset dom gated synchronous
     => Signal dom IS -> Signal dom OS
core = mealy (\s i -> swap $ cycle' s i) def

system :: (HasClockReset dom gated synchronous, KnownNat n, (n + m) ~ 15)
       => Vec (2 ^ n) Word
       -> Signal dom Word
       -> Signal dom (Maybe (WordAddr, Maybe Word))
system raminit ioin = ioout
  where
    coreOuts = core $ IS <$> mux readWasIO ioin mout <*> dout <*> rout

    bread = coreOuts <&> (^. osMRead)
    bwrite = coreOuts <&> (^. osMWrite)

    mread = bread <&> truncateB <&> unpack
    mwrite = bwrite <&> fmap (_1 %~ (unpack . truncateB))

    dread = coreOuts <&> (^. osDOp . _1) <&> unpack
    dwrite = coreOuts <&> (^. osDOp) <&> repackStack

    rread = coreOuts <&> (^. osROp . _1) <&> unpack
    rwrite = coreOuts <&> (^. osROp) <&> repackStack

    mout = blockRamPow2 raminit mread (mux addrIO (pure Nothing) mwrite)
    dout = blockRamPow2 (repeat 0) dread dwrite
    rout = blockRamPow2 (repeat 0) rread rwrite

    repackStack (_, Nothing) = Nothing
    repackStack (a, Just v) = Just (unpack a, v)

    addrIO = bread <&> (> 16383)
    readWasIO = register False addrIO

    packRW _ (Just (a, v)) = (pack a, Just v)
    packRW a Nothing = (pack a, Nothing)

    ioout = mux addrIO (Just <$> (packRW <$> bread <*> bwrite)) (pure Nothing)

topEntity c r = withClockReset @System @Source @Asynchronous c r $
  system (repeat @256 0)
