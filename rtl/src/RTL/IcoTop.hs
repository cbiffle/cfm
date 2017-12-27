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
import RTL.VGA

system :: (HasClockReset dom gated synchronous)
       => FilePath
       -> Signal dom Cell
       -> ( Signal dom Cell
          , Signal dom Bool
          , Signal dom Bool
          , Signal dom (BitVector 6)
          )
system raminit ins = (outs, hsync, vsync, vid)
  where
    (ioreq, fetch) = coreWithRAM ram ioresp

    (ioreq0 :> ioreq1 :> ioreq2 :> ioreq3 :> ioreq4 :> _, ioch) = ioDecoder @3 ioreq
    ioresp = responseMux (ioresp0 :> ioresp1 :> ioresp2 :> ioresp3 :> ioresp4 :> repeat (pure 0)) ioch

    ram r w = ramRewrite $ blockRamFile (SNat @4096) raminit r w

    -- I/O devices
    (ioresp0, outs) = outport $ partialDecode ioreq0
    (ioresp1, irq0) = inport ins ioreq1
    (ioresp2, irq1 :> irq2 :> Nil) = timer $ partialDecode @2 ioreq2
    (ramRewrite, ioresp3) = multiIrqController irqs fetch $ partialDecode ioreq3
    irqs = irq0 :> irq1 :> irq2 :> hirq :> virq :> evirq :> repeat (pure False)

    (ioresp4, hsync, vsync, hirq, virq, evirq, vid) = chargen (partialDecode ioreq4)

{-# ANN topEntity (defTop { t_name = "ico_soc"
                          , t_inputs = [ PortName "clk_core"
                                       , PortName "reset"
                                       , PortName "inport"
                                       ]
                          , t_output = PortField ""
                                       [ PortName "out1"
                                       , PortName "hsync"
                                       , PortName "vsync"
                                       , PortName "vid"
                                       ]
                          }) #-}
topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Cell
          -> ( Signal System Cell
             , Signal System Bool
             , Signal System Bool
             , Signal System (BitVector 6)
             )
topEntity c r = withClockReset c r $ system "random-4k.readmemb"
