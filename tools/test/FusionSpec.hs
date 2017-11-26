module FusionSpec where

import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Data.Bits

import InstInfo

newtype StackDelta = StackDelta Int deriving (Show)

instance Arbitrary StackDelta where
  arbitrary = StackDelta <$> elements [-2, -1, 0, 1]

newtype NNStackDelta = NNStackDelta Int deriving (Show)

instance Arbitrary NNStackDelta where
  arbitrary = NNStackDelta <$> elements [0, 1]

newtype NPStackDelta = NPStackDelta Int deriving (Show)

instance Arbitrary NPStackDelta where
  arbitrary = NPStackDelta <$> elements [-2, -1, 0]

isStackDelta i = (-2) <= i && i <= 1

alu rp tmux tn tr nm radj dadj =
  0x6000 .|.
  (if rp then 0x1000 else 0) .|.
  ((tmux .&. 0xF) `shiftL` 8) .|.
  (if tn then 0x80 else 0) .|.
  (if tr then 0x40 else 0) .|.
  (if nm then 0x20 else 0) .|.
  ((radj .&. 3) `shiftL` 2) .|.
  (dadj .&. 3)

spec :: Spec
spec = do
  context "stack adjustments" $ do
    it "Dadj fusion by addition" $ property $ \(StackDelta a) (StackDelta b) ->
      isStackDelta (a + b) && a >= (a + b) ==>
        let i1 = alu False 0 False False False 0 a
            i2 = alu False 0 False False False 0 b
            iF = alu False 0 False False False 0 (a + b)
        in checkFusion i1 i2 iF

    it "Radj fusion by addition" $ property $ \(StackDelta a) (StackDelta b) ->
      isStackDelta (a + b) && a >= (a + b) ==>
        let i1 = alu False 0 False False False a 0
            i2 = alu False 0 False False False b 0
            iF = alu False 0 False False False (a + b) 0
        in checkFusion i1 i2 iF


checkFusion i1 i2 iF =
  let ds = ["a", "b", "c", "d", "e", "f"]
      rs = ["r", "s", "t", "u", "v", "w"]
      pair = evalPair ds rs i1 i2
      fused = eval ds rs iF
  in Just fused `shouldBe` pair
