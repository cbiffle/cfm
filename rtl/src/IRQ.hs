{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Interrupt support.
module IRQ where

import Clash.Prelude hiding (Word)

import Types
import Inst

-- | A simple interrupt controller supporting a single, active-high interrupt
-- input.
--
-- The controller contains an interrupt enable/disable register.  Interrupts
-- can be enabled by writing to any location within the controller's address
-- space. They are disabled when an interrupt occurs. Currently, there is no
-- way to disable interrupts programatically.
singleIrqController
  :: (HasClockReset d g s)
  => Signal d Bool    -- ^ Interrupt input, active high, level-sensitive.
  -> Signal d Bool    -- ^ CPU fetch signal, active high.
  -> Signal d (Maybe (t, Maybe w))   -- ^ I/O bus request.
  -> ( Signal d Word -> Signal d Word
     , Signal d Word
     )  -- ^ Memory-to-CPU alteration constructor and I/O response,
        -- respectively.
singleIrqController irqS fetchS reqS = (memCtor, respS)
  where
    (entryS, respS) = mooreB datapathT datapathO (False, False)
                             (irqS, fetchS, reqS)

    -- Normally, pass mem. When entering the ISR, intervene in the next fetch
    -- cycle.
    memCtor = mux entryS (pure $ pack $ NotLit $ Call 1)

    datapathT
      :: (Bool, Bool)   -- (en, enter)
      -> (Bool, Bool, Maybe (a, Maybe w))       -- (irq, fetch, ioreq)
      -> (Bool, Bool)   -- (en', enter')
    datapathT (en, _) (irq, fetch, req) = (en', entry')
      where
        -- Interrupt entry happens on any fetch cycle where we're enabled and
        -- irq is asserted, whether or not we were entering on the previous
        -- cycle.
        entry' = fetch && en && irq
        -- Set enable when we're written. Clear it on entry.
        en' = not entry' && (written || en)
        written = case req of
          Just (_, Just _) -> True
          _                -> False

    datapathO :: (Bool, Bool) -> (Bool, Word)
    datapathO (en, entry) = (entry, zeroExtend (pack en))

