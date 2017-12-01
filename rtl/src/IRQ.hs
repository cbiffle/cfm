{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Interrupt support.
module IRQ where

import Clash.Prelude hiding (Word)
import Data.Maybe (isJust)

import Types
import Inst

-- | A simple interrupt controller supporting a single, active-high interrupt
-- input.
--
-- The 'irq' input indicates an interrupt condition when 'True'. It's
-- level-sensitive. The assumption is that the interrupt-generating peripheral
-- will *hold* it 'True' until software takes action to clear the corresponding
-- condition.
--
-- The controller also contains an interrupt enable/disable register.
-- Interrupts can be enabled by writing to any location within the controller's
-- address space. They are disabled when an interrupt occurs. Currently, there
-- is no way to disable interrupts programatically.
--
-- Interrupts are only processed during CPU fetch cycles. The vast majority of
-- CPU cycles are fetch cycles; the only exception is the second cycle of a
-- load instruction.
--
-- If the 'irq' input is 'True' by the end of a CPU fetch cycle where
-- interrupts are enabled, the controller triggers interrupt entry. This
-- consists of replacing the instruction being fetched by the CPU with a call
-- to a fixed address (2). Like any call instruction, this stacks a return
-- address so that the ISR can return to the interrupted code.
--
-- However, because the call *replaces* an instruction in the interrupted code,
-- that instruction is skipped. Before returning, the ISR must adjust the
-- return address accordingly, using code like
--
-- @r> 2 - >r@
--
-- CFM doesn't have a dedicated "return from interrupt" instruction to
-- atomically re-enable interrupts on return from an ISR. This is because a
-- standard instruction will do. The instruction needs to write to the
-- interrupt enable trigger address during the same cycle that it initiates the
-- return.  Assuming the interrupt controller is mapped at address FF00, the
-- following will do the trick:
--
-- @: reti 0xFF00 2dup/! drop ;@
--
-- Call this from tail position in an ISR to get traditional
-- return-from-interrupt behavior.
singleIrqController
  :: (HasClockReset d g s)
  => Signal d Bool    -- ^ Interrupt input, active high, level-sensitive.
  -> Signal d Bool    -- ^ CPU fetch signal, active high.
  -> Signal d (Maybe (BitVector a, Maybe Word))   -- ^ I/O bus request.
  -> ( Signal d Word -> Signal d Word
     , Signal d Word
     )  -- ^ Memory-to-CPU alteration constructor and I/O response,
        -- respectively.
singleIrqController irq fetch ioreq = (memCtor, ioresp)
  where
    -- Interrupt entry happens on any fetch cycle where we're enabled and irq
    -- is asserted.
    entry = foldl1 (liftA2 (&&)) (irq :> fetch :> enabled :> Nil)
    -- Normally, pass mem. When entering the ISR, intervene in the next fetch
    -- cycle.
    memCtor = mux entry (pure $ pack $ NotLit $ Call 1)
    -- The enabled bit is initially clear at reset, gets set in response to a
    -- trigger write, and gets cleared on interrupt entry.
    enabled = register False $ mux entry (pure False) $
                               mux trigger (pure True)
                               enabled

    -- Currently any write will enable interrupts.
    trigger = isJust . (>>= snd) <$> ioreq
    -- Any read will give the interrupt enable status.
    ioresp = zeroExtend . pack <$> enabled
