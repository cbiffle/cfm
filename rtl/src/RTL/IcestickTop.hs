{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
module RTL.IcestickTop where

import Clash.Prelude hiding (readIO, read)
import CFM.Types
import RTL.IOBus
import RTL.IRQ
import RTL.GPIO
import RTL.Core
import RTL.UART

system :: (HasClockReset dom gated synchronous)
       => FilePath
       -> Signal dom Cell
       -> Signal dom Bit  -- UART RX
       -> (Signal dom Cell, Signal dom Bit)
system raminit ins urx = (outs, utx)
  where
    (ioreq, fetch) = coreWithRAM ram ioresp

    (ioreq0 :> ioreq1 :> ioreq2 :> ioreq3 :> Nil, ioch) = ioDecoder @2 ioreq
    ioresp = responseMux (ioresp0 :> ioresp1 :> ioresp2 :> ioresp3 :> Nil) ioch

    ram = vectorMux vecfetchD . singlePorted (blockRamFile (SNat @3584) raminit)

    -- I/O devices
    (ioresp0, outs) = outport $ partialDecode ioreq0
    (ioresp1, irq0) = inport ins ioreq1
    (ioresp2, _, _, urxne, utx) = uart urx $ partialDecode ioreq2
    (_, vecfetchD, ioresp3) = multiIrqController irqs fetch $ partialDecode ioreq3
    irqs = irq0 :> urxne :> repeat (pure False)

{-# ANN topEntity (defTop { t_name = "icestick_soc"
                          , t_inputs = [ PortName "clk_core"
                                       , PortName "reset"
                                       , PortName "inport"
                                       , PortName "uart_rx"
                                       ]
                          , t_output = PortField ""
                                       [ PortName "out1"
                                       , PortName "uart_tx"
                                       ]
                          }) #-}
topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Cell
          -> Signal System Bit
          -> (Signal System Cell, Signal System Bit)
topEntity c r = withClockReset c r $ system "random-3k5.readmemb"
