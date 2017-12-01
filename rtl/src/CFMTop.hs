{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
module CFMTop where

import Clash.Prelude hiding (Word, readIO, read)
import Control.Lens hiding ((:>), (:<))
import Str
import Types
import CoreInterface
import IOBus
import IRQ
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
  -> Signal dom Word    -- ^ read response from memory
  -> Signal dom Word    -- ^ read response from I/O
  -> ( Signal dom BusReq
     , Signal dom Bool
     )  -- ^ Bus request and fetch signal, respectively.
coreWithStacks stackType mresp ioresp = (busReq, fetch)
  where
    coreOuts = core $ IS <$> mresp <*> ioresp <*> n <*> r

    busReq = coreOuts <&> (^. osBusReq)
    fetch = coreOuts <&> (^. osFetch)

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

-- | Combines 'coreWithStacks' with a RAM built from the given constructor, and
-- an I/O bridge, exposing the I/O bus.
coreWithRAM
  :: (HasClockReset dom gated synchronous)
  => StackType          -- ^ Type of stack technology.
  -> (Signal dom SAddr -> Signal dom (Maybe (SAddr, Word)) -> Signal dom Word)
    -- ^ RAM constructor
  -> Signal dom Word    -- ^ I/O read response, valid when addressed.
  -> ( Signal dom (Maybe (SAddr, Maybe Word))
     , Signal dom Bool
     ) -- ^ I/O bus outputs and fetch signal, respectively.
coreWithRAM stackType ram ioresp = (ioreq, fetch)
  where
    (busReq, fetch) = coreWithStacks stackType mresp ioresp

    -- Memory reads on a blockRam do not have an enable line, i.e. a read
    -- occurs every cycle whether we like it or not. Since the reads are not
    -- effectful, that's okay, and we route the address bits to RAM independent
    -- of the type of request to save hardware.
    mread = busReq <&> \b -> case b of
      MReq a _ -> a
      IReq a   -> a

    -- Memory writes can only occur from an MReq against MSpace.
    mwrite = busReq <&> \b -> case b of
      MReq _ (Just (MSpace, a, v)) -> Just (a, v)
      _                            -> Nothing

    -- IO requests are either IReqs (reads) or MReqs against ISpace (writes).
    ioreq = busReq <&> \b -> case b of
      IReq a                       -> Just (a, Nothing)
      MReq _ (Just (ISpace, a, v)) -> Just (a, Just v)
      _                            -> Nothing

    mresp = ram mread mwrite

system :: (HasClockReset dom gated synchronous)
       => FilePath
       -> StackType
       -> Signal dom Word
       -> (Signal dom Word, Signal dom Word)
system raminit stackType ins = (outs, outs2)
  where
    (ioreq, fetch) = coreWithRAM stackType ram ioresp

    (ioreq0 :> ioreq1 :> ioreq2 :> ioreq3 :> Nil, ioch) = ioDecoder @2 ioreq
    ioresp = responseMux (ioresp0 :> ioresp1 :> ioresp2 :> ioresp3 :> Nil) ioch

    (ramRewrite, ioresp3) = singleIrqController irq fetch ioreq3

    ram r w = ramRewrite $ blockRamFile (SNat @2048) raminit r w

    -- I/O devices
    (ioresp0, outs) = outport $ partialDecode ioreq0
    (ioresp1, irq) = inport ins ioreq1
    (ioresp2, outs2) = outport $ partialDecode ioreq2

{-# ANN topEntity (defTop { t_name = "cfm_demo_top"
                          , t_inputs = [ PortName "clk_core"
                                       , PortName "reset"
                                       , PortName "inport"
                                       ]
                          , t_output = PortField "" [ PortName "out1"
                                                    , PortName "out2"
                                                    ]
                          }) #-}
topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Word
          -> (Signal System Word, Signal System Word)
topEntity c r = withClockReset @System @'Source @'Asynchronous c r $
  system "random-2k.readmemb" RAMs
