{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module CFM.InstSpec where

import Clash.Prelude
import Test.Hspec
import Test.QuickCheck

import CFM.Inst

spec = do
  it "has a correct BitPack instance 1" $ property $ \inst ->
    unpack @Inst (pack inst) `shouldBe` inst
  it "has a correct BitPack instance 2" $ property $ \bits ->
    pack (unpack @Inst bits) `shouldBe` bits
