{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ViewPatterns #-}
module CFMTop where

import Clash.Prelude hiding (Word, v, readIO)
import Control.Lens hiding ((:>))
import Data.Bool
import Str
import Types
import GPIO
import FlopStack

core :: HasClockReset dom gated synchronous
     => Signal dom IS -> Signal dom OS
core = mealy datapath def

data StackType = Flops | RAMs

system :: (HasClockReset dom gated synchronous, KnownNat n, (n + m) ~ 15)
       => Vec (2 ^ n) Word
       -> StackType
       -> Signal dom Word
system raminit stackType = outs
  where
    coreOuts = core $ IS <$> mux readWasIO ioresp mout <*> dout <*> rout

    bread = coreOuts <&> (^. osMRead)
    bwrite = coreOuts <&> (^. osMWrite)

    mread = bread <&> truncateB <&> unpack
    mwrite = bwrite <&> fmap (_1 %~ (unpack . truncateB))

    dop = coreOuts <&> (^. osDOp)
    rop = coreOuts <&> (^. osROp)

    mout = blockRamPow2 raminit mread (mux writeIO (pure Nothing) mwrite)

    dout = case stackType of
      Flops -> flopStack d15 (dop <&> (^. _2))
                             (dop <&> (^. _3))
      RAMs  -> blockRamPow2 (repeat 0) (dop <&> (^. _1) <&> unpack)
                                       (dop <&> repackStack)

    rout = case stackType of
      Flops -> flopStack d16 (rop <&> (^. _2))
                             (rop <&> (^. _3))
      RAMs  -> blockRamPow2 (repeat 0) (rop <&> (^. _1) <&> unpack)
                                       (rop <&> repackStack)

    repackStack (_, _, Nothing) = Nothing
    repackStack (a, _, Just v) = Just (unpack a, v)

    -- The I/O space is the top 16kiW of the address space, so it's sufficient
    -- to detect when bit 14 is set. I/O is single ported, so we merge the two
    -- core ports to create one I/O port; this is correct as long as the core
    -- doesn't try to execute code from I/O.

    -- Record when I/O was addressed so we can steer the read mux on the
    -- next cycle.
    readWasIO = register False $ bread <&> ((/= 0) . slice d14 d14)

    writeIO = bwrite <&> maybe False ((/= 0) . slice d14 d14 . fst)

    (ioresp, outs) = outport $ iobus
        <&> \(split -> (m, a), w) -> bool Nothing (Just (a, w)) (unpack m)
    iobus = iointerface bread bwrite

iointerface :: Signal d WordAddr
            -> Signal d (Maybe (WordAddr, Word))
            -> Signal d (WordAddr, Maybe Word)
iointerface rd wr = repack <$> rd <*> wr
  where
    repack _ (Just (a, w)) = (a, Just w)
    repack a _ = (a, Nothing)


topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Word
topEntity c r = withClockReset @System @'Source @'Asynchronous c r $
  register 0 $
  system program RAMs

program :: Vec 16 Word
program =
  0x8000 :>               -- push zero
  -- loop begins here at address 1                                c
  0xFFFF :>               -- push literal address complement      c ~a
  0b0110011000000000 :>   -- invert it                            c a
  0b0110000000100000 :>   -- store to it without dropping counter c a
  0b0110000100000011 :>   -- drop address                         c
  0x8001 :>               -- literal 1                            c 1
  0b0110001000000011 :>   -- add                                  c'
  repeat 1                -- repeat
