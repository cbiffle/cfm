{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
module CFMTop where

import Clash.Prelude hiding (Word, readIO, read)
import Control.Lens hiding ((:>))
import Data.Maybe (fromMaybe)
import Str
import Types
import IOBus
import GPIO
import FlopStack

-- | Registered version of the core datapath.
core :: HasClockReset dom gated synchronous
     => Signal dom IS -> Signal dom OS
core = mealy datapath def

-- | Choice of stack implementation technologies.
data StackType = Flops | RAMs

-- | Combines 'core' with the selected implementation of stacks, and exposes
-- the local bus interface.
coreWithStacks
  :: (HasClockReset dom gated synchronous)
  => StackType
  -> Signal dom Word    -- ^ read response from bus
  -> ( Signal dom WordAddr
     , Signal dom (Maybe (WordAddr, Word))
     ) -- ^ read and write outputs
coreWithStacks stackType bresp = (bread, bwrite)
  where
    coreOuts = core $ IS <$> bresp <*> n <*> r

    bread = coreOuts <&> (^. osMRead)
    bwrite = coreOuts <&> (^. osMWrite)

    dop = coreOuts <&> (^. osDOp)
    rop = coreOuts <&> (^. osROp)

    n = case stackType of
      Flops -> flopStack d15 (dop <&> (^. _2))
                             (dop <&> (^. _3))
      RAMs  -> readNew (blockRamPow2 (repeat $ errorX "D"))
                       (dop <&> (^. _1) <&> unpack)
                       (dop <&> repackStack)

    r = case stackType of
      Flops -> flopStack d16 (rop <&> (^. _2))
                             (rop <&> (^. _3))
      RAMs  -> readNew (blockRamPow2 (repeat $ errorX "R"))
                       (rop <&> (^. _1) <&> unpack)
                       (rop <&> repackStack)

    repackStack (_, _, Nothing) = Nothing
    repackStack (a, _, Just v) = Just (unpack a, v)

-- | Combines 'coreWithStacks' with the requested amount of local-bus RAM and
-- an I/O bridge, exposing the I/O bus. The RAM is initialized from a file a la
-- @$readmemb@, which is intended to be used to load a random seed file from
-- @icebram@.
coreWithRAM
  :: (KnownNat ramSize, HasClockReset dom gated synchronous)
  => StackType          -- ^ Type of stack technology.
  -> SNat ramSize       -- ^ Size of RAM (need not be pow2).
  -> FilePath           -- ^ Synthesis-relative RAM initialization file path.
  -> Signal dom (Maybe Word)  -- ^ I/O read response, valid when addressed.
  -> Signal dom (Maybe (IOAddr, Maybe Word))  -- ^ I/O bus outputs
coreWithRAM stackType ramSize ramPath ioresp = ioreq
  where
    (bread, bwrite) = coreWithStacks stackType bresp

    -- The I/O response mux remembers whether an I/O read was issued last
    -- cycle, and uses this information to assert its valid signal. Thus, we
    -- can use its valid signal to control our mux between memory and I/O.
    bresp = fromMaybe <$> m <*> ioresp

    m = blockRamFile ramSize ramPath bread $ mux writeIsIO (pure Nothing) bwrite

    -- The I/O bridge generates the ioreq signal and the status signal we use
    -- to suppress memory writes.
    (ioreq, writeIsIO) = coreToIO bread bwrite

system :: (HasClockReset dom gated synchronous)
       => FilePath
       -> StackType
       -> Signal dom Word
system raminit stackType = outs
  where
    ioreq = coreWithRAM stackType (SNat @2048) raminit ioresp
    (ioreq0 :> _ :> Nil, ioch) = ioDecoder @1 ioreq
    ioresp = responseMux (ioresp0 :> pure 0 :> Nil) ioch

    -- I/O devices
    (ioresp0, outs) = outport ioreq0

topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Word
topEntity c r = withClockReset @System @'Source @'Asynchronous c r $
  register 0 $
  system "random-2k.readmemb" RAMs
