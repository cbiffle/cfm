{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
module CFMSpec where

import Clash.Prelude
import Test.Hspec
import Test.QuickCheck
import Control.Lens
import Data.Maybe (isNothing)

import CFM

spec =
  context "reset" $ do
    let inputs = errorX "inputs undefined at reset"
    it "does not write" $ property $
      isNothing $ cycle' def inputs ^. _1 . osMWrite
    it "fetches first instruction" $ property $
      cycle' def inputs ^. _1 . osMRead == Just 0
    it "starts out stack at 0" $ property $
      cycle' def inputs ^. _1 . osDOp == (0, Nothing)
    it "starts out return stack at 0" $ property $
      cycle' def inputs ^. _1 . osROp == (0, Nothing)
