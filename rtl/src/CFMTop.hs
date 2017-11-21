{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
module CFMTop where

import Clash.Prelude hiding (Word, v, readIO)
import Control.Lens hiding ((:>))
import Control.Monad (join)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
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

    mout = blockRamPow2 raminit mread (mux writeIO (pure Nothing) mwrite)
    dout = blockRamPow2 (repeat 0) dread dwrite
    rout = blockRamPow2 (repeat 0) rread rwrite

    repackStack (_, Nothing) = Nothing
    repackStack (a, Just v) = Just (unpack a, v)

    writeIO = bwrite <&> maybe False ((/= 0) . slice d14 d14 . fst)
    readIO = bread <&> slice d14 d14 <&> (/= 0)
    readWasIO = register False readIO

    packRW _ (Just (a, v)) = (pack a, Just v)
    packRW a Nothing = (pack a, Nothing)

    ioout = mux ((.|.) <$> readIO <*> writeIO)
                (Just <$> (packRW <$> bread <*> bwrite))
                (pure Nothing)

topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Word
topEntity c r = withClockReset @System @'Source @'Asynchronous c r $
  register 0 $
  system program (pure 0)
    <&> fmap snd
    <&> join
    <&> fromMaybe 0

program :: Vec 16 Word
program =
  0xD555 :>               -- push constant
  0xFFFF :>               -- push literal address complement
  0b0110011000000000 :>   -- invert it
  0b0110000000100011 :>   -- store to it
  0b0110000100000011 :>   -- drop address
  repeat 0                -- repeat
