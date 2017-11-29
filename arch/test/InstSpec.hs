{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module InstSpec where

import Clash.Prelude
import Inst
import Test.Hspec
import Test.QuickCheck

spec = do
  it "has a correct BitPack instance 1" $ property $ \inst ->
    unpack @Inst (pack inst) `shouldBe` inst
  it "has a correct BitPack instance 2" $ property $ \bits ->
    pack (unpack @Inst bits) `shouldBe` bits
