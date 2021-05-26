{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Interrupt support.
module RTL.IRQ where

import Clash.Prelude

import CFM.Types
import CFM.Inst
import RTL.Common.Strobe
import RTL.IOBus (moorep, mealyp)
import RTL.Strobes

-- | RAM mux that replaces data with a call to a vector location.
vectorMux :: Signal d (Maybe VectorFetchData)
          -> Signal d Cell -> Signal d Cell
vectorMux vf = mux (fromStrobe <$> vf) (pure $ pack $ NotLit $ Call 1)

-- | A simple interrupt controller supporting a single, active-high interrupt
-- input.
--
-- The controller contains an interrupt enable/disable register.  Interrupts
-- can be enabled by writing to any location within the controller's address
-- space. They are disabled when an interrupt occurs. Currently, there is no
-- way to disable interrupts programatically.
singleIrqController
  :: (HiddenClockResetEnable d)
  => Signal d Bool    -- ^ Interrupt input, active high, level-sensitive.
  -> Signal d Bool    -- ^ CPU fetch signal, active high.
  -> Signal d (Maybe (BitVector 1, Maybe Cell))   -- ^ I/O bus request.
  -> ( Signal d (Maybe VectorFetchAddress)
     , Signal d (Maybe VectorFetchData)
     , Signal d Cell
     )  -- ^ Vector fetch events and I/O response, respectively.
singleIrqController irqS fetchS reqS = (vfaS, vfdS, respS)
  where
    (respS, unbundle -> (sEnS, entryS)) = moorep datapathT
                                          (repeat . zeroExtend . pack . sisEn)
                                          datapathO
                                          (bundle (irqS, fetchS))
                                          reqS
    vfdS = toStrobe <$> entryS

    vfaS = toStrobe <$> (sEnS .&&. fetchS .&&. irqS)

    datapathT (SIS en _) (req, (irq, fetch)) = SIS en' entry'
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

    datapathO (SIS en enter) = (en, enter)

data SIS = SIS
  { sisEn :: Bool
    -- ^ Interrupt enable flag.
  , sisEnter :: Bool
    -- ^ Interrupt entry event strobe. Goes high on the cycle when a fetch
    -- is being replaced by a vector.
  } deriving (Show, Generic, NFDataX)

instance Default SIS where def = SIS False False

-- | An interrupt controller supporting up to 16 active-high interrupt inputs.
--
-- Interrupt inputs can be individually enabled. A high level on any enabled
-- input will interrupt the processor. The processor can determine the interrupt
-- source be reading the IRQST register.
--
-- There is also a global interrupt enable bit. It is cleared at reset, and
-- when the processor is interrupted. It can be set by software to accept
-- interrupts.
--
-- Writes to the IRQST register also serve as enable triggers. Any write sets
-- the global enable bit. The value written is ignored.
--
-- Registers:
-- 0: IRQST. Reads as a mask of active and enabled interrupts. Any write sets
--    the global enable bit.
-- 2: IRQEN. Bit 0 is the global enable bit, and can be read and written. Other
--    bits read as zero and ignore writes.
-- 4: IRQSE. A 1 written to any bit position enables the corresponding
--    interrupt. On writes, zero bits are ignored. Reads as the interrupt
--    enable mask.
-- 6: IRQCE. A 1 written to any bit position disables the corresponding
--    interrupt. On writes, zero bits are ignored. Reads as the interrupt
--    enable mask (the same as IRQSE).
multiIrqController
  :: (HiddenClockResetEnable d)
  => Vec Width (Signal d Bool)
      -- ^ Interrupt inputs, active high, level-sensitive.
  -> Signal d Bool    -- ^ CPU fetch signal, active high.
  -> Signal d (Maybe (BitVector 2, Maybe Cell))   -- ^ I/O bus request.
  -> ( Signal d (Maybe VectorFetchAddress)
     , Signal d (Maybe VectorFetchData)
     , Signal d (Maybe EnablingInterrupts)
     , Signal d Cell
     )  -- ^ Vector fetch event strobes and I/O response, respectively.
multiIrqController irqS fetchS reqS = (vfaS, vfdS, eiS, respS)
  where
    (respS, unbundle -> ( fmap toStrobe -> vfdS
                        , fmap toStrobe -> vfaS
                        , fmap toStrobe -> eiS
                        )) =
        mealyp datapathT datapathR
               (bundle (bundle irqS, fetchS))
               reqS

    datapathT s (req, (irqs, fetch)) = (s', (misEnter s, entry', ei))
      where
        s' = MIS
          { misEn = not entry' && case req of
              -- Any write to the enable-trigger register enables.
              Just (0, Just _) -> True
              -- The bottom bit of writes @ 1 gets copied into the enable bit.
              Just (1, Just v) -> bitCoerce $ lsb v
              -- Anything else leaves matters unchanged.
              _                -> misEn s

          , misStatus = maskedIrqs
          , misIEn = case req of
              Just (2, Just v) -> zipWith (||) (misIEn s) (unpack v)
              Just (3, Just v) -> zipWith (&&) (misIEn s) (map not (unpack v))
              _                -> misIEn s
          , misEnter = entry'
          }

        ei | Just (0, Just _) <- req = True
           | otherwise = False
        maskedIrqs = zipWith (&&) irqs $ misIEn s
        entry' = fetch && misEn s && or maskedIrqs

    datapathR s = pack (misStatus s) :>
                  zeroExtend (pack (misEn s)) :>
                  repeat (pack (misIEn s))


data MIS = MIS
  { misEn :: Bool
    -- ^ Global interrupt enable flag.
  , misStatus :: Vec Width Bool
    -- ^ Captured active IRQs after masking.
  , misIEn :: Vec Width Bool
    -- ^ Individual interrupt enable flags.
  , misEnter :: Bool
    -- ^ Vector fetch flag. Set during the cycle where we intercede in fetch.
  } deriving (Show, Generic, NFDataX)

instance Default MIS where
  def = MIS False (repeat False) (repeat False) False


