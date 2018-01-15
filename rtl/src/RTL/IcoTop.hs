{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
import RTL.MMU
import RTL.BootROM

import qualified RTL.UART as U

type PhysAddr = BitVector 19

system :: forall dom gated synchronous.
          (HasClockReset dom gated synchronous)
       => FilePath
       -> Signal dom Cell -- input port
       -> Signal dom Cell -- SRAM-to-host
       -> Signal dom Bit  -- UART RX
       -> ( Signal dom Cell
          , Signal dom Bool
          , Signal dom Bool
          , Signal dom (BitVector 6)
          , Signal dom PhysAddr  -- SRAM address
          , Signal dom Bool  -- SRAM write
          , Signal dom Cell  -- SRAM data
          , Signal dom Bit  -- UART TX
          )
system raminit ins sram2h urx =
  (outs, hsync, vsync, vid, sramA, sramW, h2sram, utx)
  where
    (mreq, ioreq, fetch) = coreWithStacks ram ioresp

    -- The I/O bus decoder and return response multiplexer.
    (ioreq0 :> ioreq1 :> ioreq2 :> ioreq3 :>
      ioreq4 :> ioreq5 :> ioreq6 :> _, ioch) = ioDecoder @3 ioreq
    ioresp = responseMux (ioresp0 :> ioresp1 :> ioresp2 :> ioresp3 :>
                            ioresp4 :> ioresp5 :> ioresp6 :> repeat (pure 0))
                         ioch

    -- The RAM return path to the core is affected by the Boot ROM mux and the
    -- IRQ vector insertion logic.
    ram = vectorMux vecfetchD $ bootROM d256 raminit mreq fetch sram2h

    -- The external SRAM interface, which *is* affected by the MMU.
    (sramA, sramW, h2sram) = extsram $ mmuMapper mmuMap mreq

    -- I/O devices

    -- General output.
    (ioresp0, outs) = outport $ partialDecode ioreq0

    -- General input.
    (ioresp1, irq0) = inport ins ioreq1

    -- Timer/counter.
    (ioresp2, irq1 :> irq2 :> Nil) = timer $ partialDecode @2 ioreq2

    -- IRQ controller, giving the vector fetch logic constructor.
    (vecfetchA, vecfetchD, ioresp3) = multiIrqController irqs fetch switchBack $
                                      partialDecode ioreq3
    irqs = irq0 :> irq1 :> irq2 :> hirq :> virq :> evirq :> urxne :>
           repeat (pure False)

    -- Display controller.
    (ioresp4, hsync, vsync, hirq, virq, evirq, vid) =
        chargen $ partialDecode ioreq4

    -- UART.
    (ioresp5, _, _, urxne, utx) = U.uart urx $ partialDecode ioreq5

    -- MMU, giving the memory address mapping constructor.
    (ioresp6, switchBack, mmuMap) = mmu @3 @7 vecfetchA $ partialDecode ioreq6


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
          -> Reset System 'Synchronous
          -> Signal System Cell
          -> Signal System Cell
          -> Signal System Bit
          -> ( Signal System Cell
             , Signal System Bool
             , Signal System Bool
             , Signal System (BitVector 6)
             , Signal System PhysAddr  -- SRAM address
             , Signal System Bool  -- SRAM write
             , Signal System Cell  -- SRAM data
             , Signal System Bit
             )
topEntity c r = withClockReset c r $ system "random-256.readmemb"
