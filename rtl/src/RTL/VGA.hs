{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module RTL.VGA where

import Clash.Prelude
import Data.Maybe (fromMaybe)
import Control.Lens hiding ((:>))
import Control.Arrow (second)
import CFM.Types

-------------------------------------------------------------------------------
-- Representation of timing.

-- | Phases of a horizontal or vertical timing generator.
data Phase = FrontPorch -- ^ Start of blanking interval.
           | SyncPulse  -- ^ Sync pulse.
           | BackPorch  -- ^ End of blanking interval.
           | VisibleArea  -- ^ Actual pixels.
           deriving (Show, Eq, Ord, Enum, Bounded)

-- | Phases advance cyclicly. Note that since they're a 2-bit quantity, we
-- ought to be able to do this without a carry chain. Not tested.
nextPhase :: Phase -> Phase
nextPhase = toEnum . (.&. 3) . (+ 1) . fromEnum

-- | We maintain the number of sub-cycles to spend in each state (minus 1) in a
-- simple vector, indexed by the 'Phase'. The values themselves are sized by
-- 'n' (in bits).
type Timing n = Vec 4 (BitVector n)

-- | Transition function for updating the timing structure itself. This allows
-- 'Timing' to be exposed as a set of writable registers.
timingT :: Maybe (BitVector 2, Maybe (BitVector n)) -> Timing n -> Timing n
timingT (Just (a, Just x)) = replace a x
timingT _ = id

-- | Here's an example of timing values for a common 800x600 mode.
vesa800x600x60 :: (Timing 10, Timing 10)
vesa800x600x60 = ( 39 :> 127 :> 87 :> 799 :> Nil
                 , 0 :> 3 :> 22 :> 599 :> Nil
                 )


--------------------------------------------------------------------------------
-- Timing machines and sync generation.

-- | Timing machine state, either horizontal or vertical. This machine is
-- advanced only when a gate signal allows (e.g. for vertical sync, only at
-- horizontal blanking). The timing control registers are separate and can be
-- written at any time.
data TState n = TState
  { _tsPhase :: Phase
    -- ^ Current phase of the state machine.
  , _tsCycLeft :: BitVector n
    -- ^ Cycles remaining within current phase.
  }

makeLenses ''TState

instance Default (TState n) where def = TState VisibleArea def

-- | Transition function for updating the state of a timing machine.
--
-- The argument order is flipped from our usual Moore machines because of how
-- it gets used below.
tstateT :: (KnownNat n)
        => (Bool, Timing n) -- ^ Gate and configuration.
        -> TState n
        -> TState n
tstateT (False, _) s = s  -- maintain state when gated
tstateT (_, tm) s = s & tsPhase .~ phase'
                      & tsCycLeft .~ cyc'
  where
    zero = s ^. tsCycLeft == 0
    phase = s ^. tsPhase
    next = nextPhase phase
    -- Advance the phase and reload the counter when the counter expires.
    (phase', cyc') | zero      = (next,  tm !! next)
                   | otherwise = (phase, pred (s ^. tsCycLeft))

-- | Timing machine output function.
--
-- This produces two "event" outputs and two "level" outputs.
--
-- The event outputs are 'True' for exactly one cycle at a specific event:
--
-- - Start-of-blank is 'True' during the final cycle of the visible area.
-- - End-of-blank is 'True' during the final cycle of the blanking interval.
--
-- The level outputs are 'True' while a condition holds:
--
-- - Sync is 'True' while the sync pulse should be generated.
-- - Active is 'True' while we're in the visible area of timing.
tstateO :: (KnownNat n)
        => TState n
        -> ( Bool       -- start of blank
           , Bool       -- end of blank
           , Bool       -- sync level
           , Bool       -- active
           )
tstateO s = (blank, eblank, sync, active)
  where
    blank = s ^. tsCycLeft == 0 && s ^. tsPhase == VisibleArea
    eblank = s ^. tsCycLeft == 0 && s ^. tsPhase == BackPorch
    sync = s ^. tsPhase == SyncPulse
    active = s ^. tsPhase == VisibleArea


-------------------------------------------------------------------------------
-- CRTC frame control and pixel addressing

data GState = GState
  { _gsH :: (TState 10, Timing 10)
    -- ^ Horizontal timing configuration and state.
  , _gsV :: (TState 10, Timing 10)
    -- ^ Vertical timing configuration and state.
  , _gsPixels :: BitVector 14
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
  , _gsFB :: BitVector 3
    -- ^ Font Base (TODO: this name needs work)
  , _gsAddr :: (Bit, BitVector 11)
    -- ^ Address for writing to video memory.
  , _gsReadValue :: Cell
    -- ^ Read value, registered to improve bus timing.
  }

makeLenses ''GState

instance Default GState where
  def = GState
    { _gsH = (def, fst vesa800x600x60)
    , _gsV = (def, snd vesa800x600x60)
    , _gsPixels = def
    , _gsShadowPixels = def
    , _gsChar0 = def
    , _gsHIF = False
    , _gsVIF = False
    , _gsEVIF = False
    , _gsFB = def
    , _gsAddr = def
    , _gsReadValue = def
    }

-- | Mealy function for the framegen circuit.
--
-- This is responsible for applying updates from the I/O bus to the state, and
-- generating the outputs. (Phrasing this as a Moore machine was duplicative.)
framegenT :: GState
          -> Maybe (BitVector 4, Maybe Cell)
          -> ( GState
             , ( (Bool, Bool) -- hsync, HIF
               , (Bool, Bool, Bool) -- vsync, VIF, EVIF
               , Bool -- Active
               , BitVector 14 -- pixel address
               , BitVector 3  -- font base
               , Maybe ((Bit, BitVector 11), BitVector 8)  -- write through
               , Cell -- read response
               ))
framegenT s iowr = (s', ( (hsync, s ^. gsHIF)
                        , (vsync, s ^. gsVIF, s ^. gsEVIF)
                        , act
                        , s ^. gsPixels
                        , s ^. gsFB
                        , write
                        , s ^. gsReadValue
                        ))
  where
    s' = s & gsPixels .~ pixels'
           & gsShadowPixels .~ shadowPixels'
           & gsChar0 .~ char0'
           & gsFB .~ fb'
           & gsAddr .~ addr'
           & gsReadValue .~ readValue
           -- Timing machine updates
           & gsH . _1 %~ tstateT (True, s ^. gsH . _2)
           & gsV . _1 %~ tstateT (hblank, s ^. gsV . _2)
           -- Timing register updates
           & gsH . _2 %~ timingT hwr
           & gsV . _2 %~ timingT vwr
           -- Interrupt flags: set on event, clear on acknowledge
           & gsHIF  %~ ((&& not hack)  . (|| hblank))
           & gsVIF  %~ ((&& not vack)  . (|| vblank))
           & gsEVIF %~ ((&& not evack) . (|| evblank))

    -- Timing machine output circuits.
    (hblank, _,       hsync, hactive) = tstateO (s ^. gsH . _1)
    (vblank, evblank, vsync, vactive) = tstateO (s ^. gsV . _1)
    act = hactive && vactive

    -- We expose three separate sets of registers to the bus: the horizontal and
    -- vertical timing, and our own control registers. Split requests here.
    -- TODO: this is a recurring pattern that could be better extracted.
    (hwr, vwr, rwr) = iowr & \w -> case w of
      Just (split -> (t, a), x)
        | t == 0    -> (Just (a, truncateB <$> x), Nothing, Nothing)
        | t == 1    -> (Nothing, Just (a, truncateB <$> x), Nothing)
        | otherwise -> (Nothing, Nothing, Just (t ++# a, x))
      _ -> (Nothing, Nothing, Nothing)

    -- The start-of-field event occurs at the start-of-hblank cycle during the
    -- last line of the vertical blanking interval, and is used to reset state
    -- for the coming visible field.
    startOfField = hblank && evblank

    lastGlyphSlice = s ^. gsFB == 7 -- TODO programmable

    -- Transition rules for the pixel addressing counter.
    -- TODO all these equations need optimizin'.
    pixels' | Just (0x8, Just v) <- rwr = truncateB (v `shiftL` 3)
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
    (evack, hack, vack) | Just (0x9, Just v) <- rwr = unpack (slice d2 d0 v)
                        | otherwise = (False, False, False)

    -- Font base transition rules.
    fb' | Just (0xA, Just v) <- rwr = truncateB v
          -- Allow host writes at any time.
        | startOfField = 0
          -- Reset to zero at start-of-field. This ensures correct display when
          -- the visible area isn't an integral number of glyph rows.
        | hblank = s ^. gsFB + 1
          -- Advance at hblank. This assumes rollover is desirable.
          -- TODO: programmable glyph heights will complicate this.
        | otherwise = s ^. gsFB

    -- Video memory write address transition rules.
    addr' | Just (0xB, Just v) <- rwr = unpack $ truncateB v
            -- Allow host writes at any time.
          | Just _ <- write = second (+1) $ s ^. gsAddr
            -- On any use of VWD (below), advance the address.
          | otherwise = s ^. gsAddr

    -- Detecting writes to VWD.
    write
      | Just (0xC, Just v) <- rwr = Just (s ^. gsAddr, truncateB v)
      | otherwise = Nothing

    -- Transition rules for char0, a simple register.
    char0' | Just (0xD, Just v) <- rwr = truncateB v
           | otherwise = s ^. gsChar0

    -- Bus response multiplexer.
    readValue = case fromMaybe 0 (fst <$> iowr) of
          x | x .&. 0xC == 0 ->
                zeroExtend $ (s ^. gsH . _2) !! (x .&. 3)
          x | x .&. 0xC == 4 ->
                zeroExtend $ (s ^. gsV . _2) !! (x .&. 3)
          8 -> zeroExtend $ s ^. gsPixels
          9 -> zeroExtend $ pack (s ^. gsEVIF, s ^. gsHIF, s ^. gsVIF)
          0xA -> zeroExtend $ s ^. gsFB
          0xB -> zeroExtend $ pack $ s ^. gsAddr
          0xC -> errorX "write-only register"
          0xD -> zeroExtend $ s ^. gsChar0
          _ -> errorX "undefined video register"

framegen :: (HasClockReset d g s)
         => Signal d (Maybe (BitVector 4, Maybe Cell))
         -> ( Signal d (Bool, Bool)   -- hsync, hblank interrupt
            , Signal d (Bool, Bool, Bool)   -- vsync, start of / end of irqs
            , Signal d Bool   -- active
            , Signal d (BitVector 14) -- pixel address
            , Signal d (BitVector 3)  -- glyph base
            , Signal d (Maybe ((Bit, BitVector 11), BitVector 8))  -- write through
            , Signal d Cell -- read response
            )
framegen = unbundle . mealy framegenT def


-------------------------------------------------------------------------------
-- Character generation.

chargen
  :: (HasClockReset d g s)
  => Signal d (Maybe (BitVector 4, Maybe Cell))
  -> ( Signal d Cell    -- read response
     , Signal d Bool    -- hsync
     , Signal d Bool    -- vsync
     , Signal d Bool    -- hblank IRQ
     , Signal d Bool    -- vblank IRQ
     , Signal d Bool    -- end-of-vblank IRQ
     , Signal d Bit     -- monochrome output
     )
chargen iowr = (resp, hsync'', vsync'', hblank, vblank, evblank, out'')
  where
    -- The outputs of framegen provide the first cycle.
    ( unbundle -> (hsync, hblank)
      , unbundle -> (vsync, vblank, evblank)
      , active, pixel, glyph, wrth, resp) = framegen iowr

    (charAddr, pxlAddr) = unbundle $ split <$> pixel

    ramsplit (Just ((0, a), v)) = (Just (unpack a, v), Nothing)
    ramsplit (Just ((_, a), v)) = (Nothing, Just (unpack a, v))
    ramsplit _ = (Nothing, Nothing)
    (charWr, glyphWr) = unbundle $ ramsplit <$> wrth

    -- Past the character memory we are delayed one cycle.
    char' = blockRamFilePow2 @_ @_ @11 @8 "random-2048x8.readmemb"
            (unpack <$> charAddr)
            charWr
    glyph' = register def glyph
    charf' = (++#) <$> char' <*> glyph'
    pxlAddr' = register def pxlAddr
    hsync' = register False hsync
    vsync' = register False vsync
    active' = register False active

    -- Past the glyph memory we're delayed another cycle.
    gslice'' = blockRamFilePow2 @_ @_ @11 @8 "font-8x8.readmemb"
               (unpack <$> charf')
               glyphWr
    pxlAddr'' = register def pxlAddr'
    hsync'' = register False hsync'
    vsync'' = register False vsync'
    active'' = register False active'
    out'' = mux active''
                ((!) <$> gslice'' <*> pxlAddr'')
                (pure 0)
