{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module RTL.VGA where

import Clash.Prelude
import Control.Arrow (second)
import CFM.Types
import RTL.IOBus (ioDecoder, responseMux)
import RTL.VGA.Palette
import RTL.VGA.Timing
import RTL.VGA.FrameGen


chargen
  :: (HiddenClockResetEnable d)
  => Signal d (Maybe (BitVector 5, Maybe Cell))
  -> ( Signal d Cell    -- read response
     , Signal d Bool    -- hsync
     , Signal d Bool    -- vsync
     , Signal d Bool    -- hblank IRQ
     , Signal d Bool    -- vblank IRQ
     , Signal d Bool    -- end-of-vblank IRQ
     , Signal d (BitVector 6)
     )
chargen ioreq = ( resp
                , hsync''
                , vsync''
                , hblank
                , vblank
                , evblank
                , out''
                )
  where
    (ioreq1 :> preq :> Nil, ioch0) = ioDecoder ioreq
    (ioreq2 :> lreq :> Nil, ioch1) = ioDecoder ioreq1
    (htreq :> vtreq :> Nil, ioch2) = ioDecoder ioreq2

    resp2 = responseMux (htresp :> vtresp :> Nil) ioch2
    resp1 = responseMux (resp2 :> lresp :> Nil) ioch1
    resp = responseMux (resp1 :> presp :> Nil) ioch0

    -- The outputs of framegen provide the first cycle.
    (htresp, hts) = timing (fst vesa800x600x56) (pure True) htreq
    (vtresp, vts) = timing (snd vesa800x600x56)
                           (timsigStartOfBlank <$> hts)
                           vtreq
    hsync   = timsigSyncActive <$> hts
    vsync   = timsigSyncActive <$> vts
    
    active = (timsigVidActive <$> hts) .&&. (timsigVidActive <$> vts)
    
    ( hblank
      , unbundle -> (vblank, evblank)
      , pixel, glyph, wrth, cursor, lresp) = framegen lreq hts vts

    (charAddr, pxlAddr) = unbundle $ split <$> pixel

    -- Split up the writethrough to the two separate RAMs.
    -- TODO: I feel like I've written this function a dozen times.
    ramsplit (Just ((0, a), v)) = (Just (unpack a, v), Nothing)
    ramsplit (Just ((_, a), v)) = (Nothing, Just (unpack a, v))
    ramsplit _ = (Nothing, Nothing)
    -- And register the writethrough path, since latency doesn't
    -- really matter.
    (charWr, glyphWr) = unbundle $ ramsplit <$> register def wrth

    -- Past the character memory we are delayed one cycle.
    achar' = blockRamFilePow2 @_ @11 @16 "rtl/syn/random-2k.readmemb"
             (unpack <$> charAddr)
             (fmap (second truncateB) <$> charWr)
    (foreI', backI', char') = ( slice d15 d12 <$> achar'
                              , slice d11 d8 <$> achar'
                              , slice d6 d0 <$> achar'  -- drop bit 7
                              )
    glyph' = register def glyph
    charf' = (++#) <$> glyph' <*> char'
    pxlAddr' = register def pxlAddr
    hsync' = register False hsync
    vsync' = register False vsync
    active' = register False active
    cursor' = register False cursor

    -- Past the glyph memory we're delayed another cycle.
    gslice'' = blockRamFilePow2 @_ @11 @8 "rtl/syn/font-8x16.readmemb"
               (unpack <$> charf')
               (fmap (second truncateB) <$> glyphWr)
    pxlAddr'' = register def pxlAddr'
    hsync'' = register False hsync'
    vsync'' = register False vsync'
    active'' = register False active'
    foreI'' = register def foreI'
    backI'' = register def backI'
    cursor'' = register False cursor'

    (fore'' :> back'' :> _, presp) = palette (foreI'' :> backI'' :> Nil) preq

    cursbits'' = mux cursor'' (pure 3) (pure 0)
    gcslice'' = (.|.) <$> gslice'' <*> cursbits''

    out'' = mux active''
                (mux (bitCoerce <$> ((!) <$> gcslice'' <*> pxlAddr''))
                     fore''
                     back'')
                (pure 0)
