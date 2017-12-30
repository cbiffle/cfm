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
import RTL.SRAM

import qualified RTL.UART as U

system :: (HasClockReset dom gated synchronous)
       => FilePath
       -> Signal dom Cell -- input port
       -> Signal dom Cell -- SRAM-to-host
       -> Signal dom Bit  -- UART RX
       -> ( Signal dom Cell
          , Signal dom Bool
          , Signal dom Bool
          , Signal dom (BitVector 6)
          , Signal dom (BitVector 13)  -- SRAM address
          , Signal dom Bool  -- SRAM write
          , Signal dom Cell  -- SRAM data
          , Signal dom Bit  -- UART TX
          )
system raminit ins sram2h urx = (outs, hsync, vsync, vid, sramA, sramW, h2sram, utx)
  where
    (ioreq, fetch) = coreWithRAM ram ioresp

    (ioreqSram :> ioreqOthers :> Nil, ioch0) = ioDecoder @1 ioreq
    ioresp = responseMux (iorespSram :> iorespOthers :> Nil) ioch0

    (ioreq0 :> ioreq1 :> ioreq2 :> ioreq3 :> ioreq4 :> ioreq5 :> _, ioch1) = ioDecoder @3 ioreqOthers
    iorespOthers = responseMux (ioresp0 :> ioresp1 :> ioresp2 :> ioresp3 :> ioresp4 :> ioresp5 :> repeat (pure 0)) ioch1

    ram r w = ramRewrite $ blockRamFile (SNat @4096) raminit r w

    (iorespSram, sramA, sramW, h2sram) = extsram sram2h ioreqSram

    -- I/O devices
    (ioresp0, outs) = outport $ partialDecode ioreq0
    (ioresp1, irq0) = inport ins ioreq1
    (ioresp2, irq1 :> irq2 :> Nil) = timer $ partialDecode @2 ioreq2
    (ramRewrite, ioresp3) = multiIrqController irqs fetch $ partialDecode ioreq3
    irqs = irq0 :> irq1 :> irq2 :> hirq :> virq :> evirq :> urxne :> repeat (pure False)

    (ioresp4, hsync, vsync, hirq, virq, evirq, vid) = chargen (partialDecode ioreq4)

    (ioresp5, _, _, urxne, utx) = U.uart urx $ partialDecode ioreq5

{-# ANN topEntity (defTop { t_name = "ico_soc"
                          , t_inputs = [ PortName "clk_core"
                                       , PortName "reset"
                                       , PortName "inport"
                                       , PortName "sram_to_host"
                                       , PortName "uart_rx"
                                       ]
                          , t_output = PortField ""
                                       [ PortName "out1"
                                       , PortName "hsync"
                                       , PortName "vsync"
                                       , PortName "vid"
                                       , PortName "sram_a"
                                       , PortName "sram_wr"
                                       , PortName "host_to_sram"
                                       , PortName "uart_tx"
                                       ]
                          }) #-}
topEntity :: Clock System 'Source
          -> Reset System 'Asynchronous
          -> Signal System Cell
          -> Signal System Cell
          -> Signal System Bit
          -> ( Signal System Cell
             , Signal System Bool
             , Signal System Bool
             , Signal System (BitVector 6)
             , Signal System (BitVector 13)  -- SRAM address
             , Signal System Bool  -- SRAM write
             , Signal System Cell  -- SRAM data
             , Signal System Bit
             )
topEntity c r = withClockReset c r $ system "random-4k.readmemb"
