{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
module RTL.IcoTop where

import Clash.Prelude hiding (readIO, read)
import CFM.Types
import RTL.IOBus
import RTL.IRQ
import RTL.GPIO
import RTL.Timer
import RTL.Core

system :: (HasClockReset dom gated synchronous)
       => FilePath
       -> Signal dom Cell
       -> Signal dom Cell
system raminit ins = outs
  where
    (ioreq, fetch) = coreWithRAM ram ioresp

    (ioreq0 :> ioreq1 :> ioreq2 :> ioreq3 :> Nil, ioch) = ioDecoder @2 ioreq
    ioresp = responseMux (ioresp0 :> ioresp1 :> ioresp2 :> ioresp3 :> Nil) ioch

    ram r w = ramRewrite $ blockRamFile (SNat @3072) raminit r w

    -- I/O devices
    (ioresp0, outs) = outport $ partialDecode ioreq0
    (ioresp1, irq0) = inport ins ioreq1
    (irq1 :> irq2 :> Nil, ioresp2) = timer $ partialDecode @2 ioreq2
    (ramRewrite, ioresp3) = multiIrqController irqs fetch $ partialDecode ioreq3
    irqs = irq0 :> irq1 :> irq2 :> repeat (pure False)

{-# ANN topEntity (defTop { t_name = "ico_soc"
                          , t_inputs = [ PortName "clk_core"
                                       , PortName "reset"
                                       , PortName "inport"
                                       ]
                          , t_output = PortName "out1"
                          }) #-}
topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Cell
          -> Signal System Cell
topEntity c r = withClockReset c r $ system "random-3k.readmemb"
