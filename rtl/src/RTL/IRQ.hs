{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Interrupt support.
module RTL.IRQ where

import Clash.Prelude

import CFM.Types
import CFM.Inst

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
  -> ( Signal d Cell -> Signal d Cell
     , Signal d Cell
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

    datapathO :: (Bool, Bool) -> (Bool, Cell)
    datapathO (en, entry) = (entry, zeroExtend (pack en))

data MIS = MIS
  { misEn :: Bool
  , misStatus :: Vec Width Bool
  , misIEn :: Vec Width Bool
  , misAddr :: BitVector 2
  , misEnter :: Bool
  } deriving (Show)

instance Default MIS where
  def = MIS False (repeat False) (repeat False) 0 False

-- Registers:
-- 0: interrupt statuses on read; enable trigger on write
-- 2: literal enable bit (read/write)
-- 4: interrupt mask set enable
-- 6: interrupt mask clear enable
multiIrqController
  :: (HasClockReset d g s)
  => Vec Width (Signal d Bool)
      -- ^ Interrupt inputs, active high, level-sensitive.
  -> Signal d Bool    -- ^ CPU fetch signal, active high.
  -> Signal d (Maybe (BitVector 2, Maybe Cell))   -- ^ I/O bus request.
  -> ( Signal d Cell -> Signal d Cell
     , Signal d Cell
     )  -- ^ Memory-to-CPU alteration constructor and I/O response,
        -- respectively.
multiIrqController irqS fetchS reqS = (memCtor, respS)
  where
    (entryS, respS) = mooreB datapathT datapathO def
                             (bundle irqS, fetchS, reqS)

    -- Normally, pass mem. When entering the ISR, intervene in the next fetch
    -- cycle.
    memCtor = mux entryS (pure $ pack $ NotLit $ Call 1)

    datapathT s (irqs, fetch, req) = s'
      where
        s' = MIS
          { misEn = not entry' && case req of
              -- Any write to the enable-trigger register enables.
              Just (0, Just _) -> True
              -- The bottom bit of writes @ 1 gets copied into the enable bit.
              Just (1, Just v) -> unpack $ lsb v
              -- Anything else leaves matters unchanged.
              _                -> misEn s

          , misStatus = maskedIrqs
          , misIEn = case req of
              Just (2, Just v) -> zipWith (||) (misIEn s) (unpack v)
              Just (3, Just v) -> zipWith (&&) (misIEn s) (map not (unpack v))
              _                -> misIEn s
          , misAddr = case req of
              Just (a, Nothing) -> a
              _                 -> misAddr s
          , misEnter = entry'
          }

        maskedIrqs = zipWith (&&) irqs $ misIEn s
        entry' = fetch && misEn s && foldl1 (||) maskedIrqs

    datapathO s = (misEnter s, resp)
      where
        resp = case misAddr s of
          0 -> pack $ misStatus s
          1 -> zeroExtend $ pack $ misEn s
          2 -> pack $ misIEn s
          _ -> pack $ misIEn s

