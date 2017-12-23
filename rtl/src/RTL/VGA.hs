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
-- Timing and sync generation

-- | Timing machine state, either horizontal or vertical. This contains the
-- state with gated updates; the timing control registers, which can be written
-- at any time, are external.
data TState n = TState
  { _tsPhase :: Phase
    -- ^ Current phase of the state machine.
  , _tsCycLeft :: BitVector n
    -- ^ Cycles remaining within current phase.
  }

data Phase = FrontPorch
           | SyncPulse
           | BackPorch
           | VisibleArea
           deriving (Show, Eq, Ord, Enum, Bounded)

makeLenses ''TState

-- | Phases advance cyclicly. Note that since they're a 2-bit quantity, we
-- ought to be able to do this without a carry chain. Not tested.
nextPhase :: Phase -> Phase
nextPhase = toEnum . (.&. 3) . (+ 1) . fromEnum

instance Default (TState n) where def = TState VisibleArea def

-- | We maintain the number of sub-cycles to spend in each state in a simple
-- vector, indexed by the 'Phase'. The values themselves are sized by 'n'.
type Timing n = Vec 4 (BitVector n)

vesa800x600x60 :: (Timing 10, Timing 10)
vesa800x600x60 = ( 39 :> 127 :> 87 :> 799 :> Nil
                 , 0 :> 3 :> 22 :> 599 :> Nil
                 )

timingT :: Maybe (BitVector 2, Maybe (BitVector n)) -> Timing n -> Timing n
timingT (Just (a, Just x)) = replace a x
timingT _ = id

-- | Transition function for updating the state of a timing machine.
tstateT :: (KnownNat n)
        => TState n
        -> (Bool, Timing n)
        -> TState n
tstateT s (False, _) = s  -- maintain state when gated
tstateT s (_, tm) = s & tsPhase .~ phase'
                      & tsCycLeft .~ cyc'
  where
    zero = s ^. tsCycLeft == 0
    phase = s ^. tsPhase
    next = nextPhase phase
    phase' | zero = next
           | otherwise = phase
    cyc' | zero = tm !! next
         | otherwise = pred (s ^. tsCycLeft)

-- | Timing machine output function.
--
-- The blanking output signal can be used to produce interrupts, and also to
-- gate the vertical machine from the horizontal.
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
  def = GState (def, fst vesa800x600x60)
               (def, snd vesa800x600x60)
               def def def False False False def def def

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
                        , vid
                        , s ^. gsFB
                        , write
                        , s ^. gsReadValue
                        ))
  where
    s' = s & gsH . _1 %~ flip tstateT (True, s ^. gsH . _2)
           & gsH . _2 %~ timingT hwr
           & gsV . _1 %~ flip tstateT (hblank, s ^. gsV . _2)
           & gsV . _2 %~ timingT vwr
           & gsPixels .~ pixels'
           & gsShadowPixels .~ shadowPixels'
           & gsChar0 .~ char0'
           & gsHIF %~ ((&& not hack) . (|| hblank))
           & gsVIF %~ ((&& not vack) . (|| vblank))
           & gsEVIF %~ ((&& not evack) . (|| evblank))
           & gsFB .~ fb'
           & gsAddr .~ addr'
           & gsReadValue .~ readValue

    iosplit (Just (split -> (t, a), x)) = case t of
      0 -> (Just (a, truncateB <$> x), Nothing, Nothing)
      1 -> (Nothing, Just (a, truncateB <$> x), Nothing)
      _ -> (Nothing, Nothing, Just (t ++# a, x))
    iosplit _ = (Nothing, Nothing, Nothing)

    (hwr, vwr, rwr) = iosplit iowr

    startOfField = hblank && evblank
    lastGlyphSlice = s ^. gsFB == 7 -- TODO programmable

    -- TODO all these equations need optimizin'.
    pixels' | Just (0x8, Just v) <- rwr = truncateB (v `shiftL` 3)
            | startOfField = (s ^. gsChar0) ++# 0
            | vactive && hblank && not lastGlyphSlice = s ^. gsShadowPixels
            | hactive && vactive = s ^. gsPixels + 1
            | otherwise = s ^. gsPixels

    shadowPixels' | vactive && hblank && lastGlyphSlice = s ^. gsPixels + 1
                  | startOfField = (s ^. gsChar0) ++# 0
                  | otherwise = s ^. gsShadowPixels

    (evack, hack, vack) | Just (0x9, Just v) <- rwr = unpack (slice d2 d0 v)
                        | otherwise = (False, False, False)

    fb' | Just (0xA, Just v) <- rwr = truncateB v
        | startOfField = 0
        | hblank = s ^. gsFB + 1
        | otherwise = s ^. gsFB

    (hblank, _, hsync, hactive) = tstateO (s ^. gsH . _1)
    (vblank, evblank, vsync, vactive) = tstateO (s ^. gsV . _1)
    vid = s ^. gsPixels
    act = hactive && vactive

    addr' | Just (0xB, Just v) <- rwr = unpack $ truncateB v
          | Just _ <- write = second (+1) $ s ^. gsAddr -- autoincrement on use
          | otherwise = s ^. gsAddr

    write
      | Just (0xC, Just v) <- rwr = Just (s ^. gsAddr, truncateB v)
      | otherwise = Nothing

    char0' | Just (0xD, Just v) <- rwr = truncateB v
          | otherwise = s ^. gsChar0

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
