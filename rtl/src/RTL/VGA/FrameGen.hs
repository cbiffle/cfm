{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module RTL.VGA.FrameGen (framegen) where

import Clash.Prelude
import GHC.Generics
import Data.Maybe (fromMaybe)
import Control.Lens hiding ((:>))
import Control.Arrow (second)
import Control.DeepSeq
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import CFM.Types
import RTL.VGA.Timing (TimingSigs(..))

data GState = GState
  { _gsPixels :: BitVector 14
    -- ^ Pixel addressing counter.
  , _gsShadowPixels :: BitVector 14
    -- ^ Shadow of gsPixels used to effect character retracing.
  , _gsChar0 :: BitVector 11
    -- ^ Offset of top-left corner of display in character RAM. Used to reset
    -- the pixel addressing counter at top of frame.
  , _gsHIF :: Bool
    -- ^ Hblank Interrupt Flag
  , _gsVIF :: Bool
    -- ^ Vblank Interrupt Flag
  , _gsEVIF :: Bool
    -- ^ End-of-Vblank Interrupt Flag
  , _gsFB :: BitVector 4
    -- ^ Font Base (TODO: this name needs work)
  , _gsAddr :: (Bit, BitVector 11)
    -- ^ Address for writing to video memory.
  , _gsReadValue :: Cell
    -- ^ Read value, registered to improve bus timing.
  }
  deriving (Show, Generic, NFData)

makeLenses ''GState

instance Default GState where
  def = GState
    { _gsPixels = def
    , _gsShadowPixels = def
    , _gsChar0 = def
    , _gsHIF = False
    , _gsVIF = False
    , _gsEVIF = False
    , _gsFB = def
    , _gsAddr = def
    , _gsReadValue = def
    }

instance Arbitrary GState where
  arbitrary = genericArbitrary

-- | The frame generator coordinates with two timing machines to produce
-- control signals for driving the character generator. It is roughly analogous
-- to an MC6845.
framegen :: (HasClockReset d g s)
         => Signal d (Maybe (BitVector 3, Maybe Cell))
         -> Signal d TimingSigs
         -> Signal d TimingSigs
         -> ( Signal d Bool   -- hblank interrupt
            , Signal d (Bool, Bool)   -- start of / end of irqs
            , Signal d Bool   -- active
            , Signal d (BitVector 14) -- pixel address
            , Signal d (BitVector 4)  -- glyph base
            , Signal d (Maybe ((Bit, BitVector 11), Cell))  -- write through
            , Signal d Bool -- cursor
            , Signal d Cell -- read response
            )
framegen ioreq hts vts = mealyB framegenT def (ioreq, bundle (hts, vts))

-- | Mealy function for the framegen circuit.
--
-- This is responsible for applying updates from the I/O bus to the state, and
-- generating the outputs. (Phrasing this as a Moore machine was duplicative.)
framegenT :: GState
          -> ( Maybe (BitVector 3, Maybe Cell)
             , ( TimingSigs
               , TimingSigs
               )
             )
          -> ( GState
             , ( Bool -- HIF
               , (Bool, Bool) -- VIF, EVIF
               , Bool -- Active
               , BitVector 14 -- pixel address
               , BitVector 4  -- font base
               , Maybe ((Bit, BitVector 11), Cell)  -- write through
               , Bool  -- cursor active?
               , Cell -- read response
               ))
framegenT s (iowr, (hts, vts)) =
  (s', ( s ^. gsHIF
       , (s ^. gsVIF, s ^. gsEVIF)
       , act
       , s ^. gsPixels
       , s ^. gsFB
       , write
       , s ^. gsAddr == (0, slice d13 d3 (s ^. gsPixels))
       , s ^. gsReadValue
       ))
  where
    s' = s & gsPixels .~ pixels'
           & gsShadowPixels .~ shadowPixels'
           & gsChar0 .~ char0'
           & gsFB .~ fb'
           & gsAddr .~ addr'
           & gsReadValue .~ readValue
           -- Interrupt flags: set on event, clear on acknowledge
           & gsHIF  %~ ((&& not hack)  . (|| hblank))
           & gsVIF  %~ ((&& not vack)  . (|| vblank))
           & gsEVIF %~ ((&& not evack) . (|| evblank))

    -- Timing machine output circuits.
    TimingSigs hblank _ _ hactive = hts
    TimingSigs vblank evblank _ vactive = vts
    act = hactive && vactive

    -- The start-of-field event occurs at the start-of-hblank cycle during the
    -- last line of the vertical blanking interval, and is used to reset state
    -- for the coming visible field.
    startOfField = hblank && evblank

    lastGlyphSlice = s ^. gsFB == 0xF -- TODO programmable

    -- Transition rules for the pixel addressing counter.
    -- TODO all these equations need optimizin'.
    pixels' | Just (0, Just v) <- iowr = truncateB (v `shiftL` 3)
              -- Allow host writes at any time.
            | startOfField = (s ^. gsChar0) ++# 0
              -- Reset to char0 at start-of-field.
            | vactive && hblank && not lastGlyphSlice = s ^. gsShadowPixels
              -- For all rows of a glyph save the last, reset to the shadow
              -- at hblank.
            | hactive && vactive = s ^. gsPixels + 1
              -- Increment when active (includes final hblank of glyph).
            | otherwise = s ^. gsPixels

    -- Transition rules for the shadow pixel addressing counter.
    shadowPixels' | vactive && hblank && lastGlyphSlice = s ^. gsPixels + 1
                    -- Take new value at hblank on final row of glyph.
                  | startOfField = (s ^. gsChar0) ++# 0
                    -- Reset to char0 at start-of-field.
                  | otherwise = s ^. gsShadowPixels

    -- Interrupt acknowledge signals, detected when the host writes 1 to bits in
    -- register 0x12.
    (evack, hack, vack) | Just (1, Just v) <- iowr = unpack (slice d2 d0 v)
                        | otherwise = (False, False, False)

    -- Font base transition rules.
    fb' | Just (2, Just v) <- iowr = truncateB v
          -- Allow host writes at any time.
        | startOfField = 0
          -- Reset to zero at start-of-field. This ensures correct display when
          -- the visible area isn't an integral number of glyph rows.
        | hblank = s ^. gsFB + 1
          -- Advance at hblank. This assumes rollover is desirable.
          -- TODO: programmable glyph heights will complicate this.
        | otherwise = s ^. gsFB

    -- Video memory write address transition rules.
    addr' | Just (3, Just v) <- iowr = unpack $ truncateB v
            -- Allow host writes at any time.
          | Just _ <- write = second (+1) $ s ^. gsAddr
            -- On any use of VWD (below), advance the address.
          | otherwise = s ^. gsAddr

    -- Detecting writes to VWD.
    write
      | Just (4, Just v) <- iowr = Just (s ^. gsAddr, v)
      | otherwise = Nothing

    -- Transition rules for char0, a simple register.
    char0' | Just (5, Just v) <- iowr = truncateB v
           | otherwise = s ^. gsChar0

    -- Bus response multiplexer.
    readValue = case fromMaybe 0 (fst <$> iowr) of
          0 -> zeroExtend $ s ^. gsPixels
          1 -> zeroExtend $ pack (s ^. gsEVIF, s ^. gsHIF, s ^. gsVIF)
          2 -> zeroExtend $ s ^. gsFB
          3 -> zeroExtend $ pack $ s ^. gsAddr
          4 -> errorX "write-only register"
          5 -> zeroExtend $ s ^. gsChar0
          _ -> errorX "undefined video register"


