{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module RTL.IOBusSpec where

import Clash.Prelude
import qualified Data.List as L
import Test.Hspec
import Test.QuickCheck

import CFM.Types
import RTL.IOBus

spec = do
  context "ioDecoder' at various type indices" $ do
    ioDecoderDatapathSpec d1 d4
    ioDecoderDatapathSpec d2 d4
    ioDecoderDatapathSpec d3 d4
    ioDecoderDatapathSpec d4 d0

  context "responseMux at various type indices" $ do
    responseMuxSpec d0
    responseMuxSpec d1
    responseMuxSpec d2

ioDecoderDatapathSpec
  :: forall m n. (KnownNat m, KnownNat n, CmpNat m 0 ~ 'GT)
  => SNat m -> SNat n -> Spec
ioDecoderDatapathSpec mS nS =
  context ("ioDecoder' " L.++ show mS L.++ " " L.++ show nS) $ do
    let special = ioDecoder' @m @n @()

    it "selects no output when input is Nothing" $ 
      special Nothing `shouldBe` (repeat Nothing, Nothing)
 
    it "selects output corresponding to top bits of input" $ property $ \a d ->
      let t = topBits @m @n a
          expOuts = replace t (Just (truncateB a, d)) $ repeat Nothing
      in fst (special (Just (a, d))) `shouldBe` expOuts

    it "yields the selected channel on reads" $ property $ \a ->
      let t = topBits @m @n a
      in snd (special (Just (a, Nothing))) `shouldBe` Just t

    it "does not yield a channel on writes" $ property $ \a d ->
      snd (special (Just (a, Just d))) `shouldBe` Nothing

responseMuxSpec
  :: forall m. (KnownNat m)
  => SNat m -> Spec
responseMuxSpec mS = context ("responseMux " L.++ show mS) $ do
  let special = withClockResetEnable systemClockGen systemResetGen enableGen $
                responseMux @m @(BitVector m)
      sim :: [Vec (2^m) (BitVector m)] -> [Maybe (BitVector m)]
          -> [BitVector m]
      sim [] [] = []
      sim (i0:inputs') (ch0:ch') =
            -- reset is only de-asserted after the first active edge,
            -- so duplicate the first sample
        let inputs = i0:i0:inputs'
            ch = ch0:ch0:ch'
            ix = indices (d2 `powSNat` mS)
            sep = map (\i -> L.map (!! i) inputs) ix
            inputsS = map fromList sep
            chS = fromList ch
        in sampleN (L.length inputs) (special inputsS chS)

  it "routes data after ch=Just x" $ property $
    \i0 ch0 i1 ch1 -> L.last (sim [i0, i1] [Just ch0, ch1])
        `shouldBe` (i1 !! ch0)
