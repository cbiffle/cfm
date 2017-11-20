{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module CFMSpec where

import Clash.Prelude
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Control.Lens
import Data.Maybe (isNothing)

import CFM

newtype Fetch = Fetch MS deriving (Show)

instance Arbitrary Fetch where
  arbitrary = Fetch <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> pure False)

newtype Load = Load MS deriving (Show)

instance Arbitrary Load where
  arbitrary = Load <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> pure True)

spec = do
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

  context "load flag behavior" $ do
    let u = errorX "must not be used"
        go s x = cycle' s (IS x u u)
        test p l = \(Load s) -> p $ go s u ^. l
    it "doesn't write Memory" $ property $ test isNothing (_1 . osMWrite)
    it "doesn't write Return" $ property $ test isNothing (_1 . osROp . _2)
    it "doesn't write Data" $ property $ test isNothing (_1 . osDOp . _2)
    it "fetches next" $ property $ \(Load s) ->
      go s u ^. _1 . osMRead == Just (s ^. msPC + 1)
    it "addresses D" $ property $ \(Load s) ->
      go s u ^. _1 . osDOp . _1 == s ^. msDPtr
    it "addresses R" $ property $ \(Load s) ->
      go s u ^. _1 . osROp . _1 == s ^. msRPtr

    let stdelta f l = \(Load s) -> (go s u ^. _2 . l) == f (s ^. l)

    it "clears load flag" $ property $ stdelta (const False) msLoadFlag
    it "preserves DPtr" $ property $ stdelta id msDPtr
    it "preserves RPtr" $ property $ stdelta id msRPtr
    it "advances PC" $ property $ stdelta (+1) msPC

    it "loads T" $ property $ \(Load s) v ->
      go s v ^. _2 . msT == v

  context "literal push" $ do
    let mklit :: BitVector 15 -> BitVector 16
        mklit x = 1 ++# x
        go s x = cycle' s (IS (mklit x) u u)
        u = errorX "must not be used in this test"
        test p l = \x (Fetch s) -> p $ go s x ^. l
    -- Black-box (outputs-based) properties
    it "doesn't write Memory" $ property $ test isNothing (_1 . osMWrite)
    it "doesn't write Return" $ property $ test isNothing (_1 . osROp . _2)

    it "flushes T" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osDOp == (s ^. msDPtr + 1, Just (s ^. msT))

    -- White-box (microarchitectural) properties
    let stdelta f l = \x (Fetch s) -> (go s x ^. _2 . l) == f (s ^. l)

    it "doesn't set LF" $ property $ stdelta id msLoadFlag
    it "increments DPtr" $ property $ stdelta (+1) msDPtr
    it "preserves RPtr" $ property $ stdelta id msRPtr
    it "advances PC" $ property $ stdelta (+1) msPC
    it "loads T" $ property $ \x (Fetch s) ->
      go s x ^. _2 . msT == zeroExtend x
    it "fetches next" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osMRead == Just (s ^. msPC + 1)
    it "addresses D" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osDOp . _1 == s ^. msDPtr + 1
    it "addresses R" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osROp . _1 == s ^. msRPtr

  context "jump" $ do
    let mkjmp :: BitVector 13 -> BitVector 16
        mkjmp x = 0 ++# x
        go s x = cycle' s (IS (mkjmp x) u u)
        u = errorX "must not be used in this test"
        test p l = \x (Fetch s) -> p $ go s x ^. l
    -- Black-box (outputs-based) properties
    it "doesn't write Memory" $ property $ test isNothing (_1 . osMWrite)
    it "doesn't write Return" $ property $ test isNothing (_1 . osROp . _2)
    it "doesn't write Data" $ property $ test isNothing (_1 . osDOp . _2)

    -- White-box (microarchitectural) properties
    let stdelta f l = \x (Fetch s) -> (go s x ^. _2 . l) == f (s ^. l)

    it "doesn't set LF" $ property $ stdelta (const False) msLoadFlag
    it "preserves DPtr" $ property $ stdelta id msDPtr
    it "preserves RPtr" $ property $ stdelta id msRPtr
    it "preserves T" $ property $ stdelta id msT
    it "advances PC" $ property $ \x (Fetch s) ->
      go s x ^. _2 . msPC == zeroExtend x
    it "fetches next" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osMRead == Just (zeroExtend x)
    it "addresses D" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osDOp . _1 == s ^. msDPtr
    it "addresses R" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osROp . _1 == s ^. msRPtr

  context "conditional jump" $ do
    let mkjmp :: BitVector 13 -> BitVector 16
        mkjmp x = 0b001 ++# x
        go s x d = cycle' s (IS (mkjmp x) d u)
        u = errorX "must not be used in this test"
        test p l = \x (Fetch s) d -> p $ go s x d ^. l
    -- Black-box (outputs-based) properties
    it "doesn't write Memory" $ property $ test isNothing (_1 . osMWrite)
    it "doesn't write Return" $ property $ test isNothing (_1 . osROp . _2)
    it "doesn't write Data" $ property $ test isNothing (_1 . osDOp . _2)

    -- White-box (microarchitectural) properties
    let stdelta f l = \x (Fetch s) d -> (go s x d ^. _2 . l) == f (s ^. l)

    it "doesn't set LF" $ property $ stdelta id msLoadFlag
    it "decrements DPtr" $ property $ stdelta (+ 0xFFFF) msDPtr
    it "preserves RPtr" $ property $ stdelta id msRPtr
    it "pops T" $ property $ \x (Fetch s) d ->
      go s x d ^. _2 . msT == d
      
    it "advances PC" $ property $ \x (Fetch s) d ->
      go s x d ^. _2 . msPC == if s ^. msT == 0
                                 then zeroExtend x
                                 else s ^. msPC + 1
    it "fetches next" $ property $ \x (Fetch s) d ->
      go s x d ^. _1 . osMRead == if s ^. msT == 0
                                    then Just (zeroExtend x)
                                    else Just (s ^. msPC + 1)
    it "addresses D" $ property $ \x (Fetch s) d ->
      go s x d ^. _1 . osDOp . _1 == s ^. msDPtr - 1
    it "addresses R" $ property $ \x (Fetch s) d ->
      go s x d ^. _1 . osROp . _1 == s ^. msRPtr

  context "call" $ do
    let mkcall :: BitVector 13 -> BitVector 16
        mkcall x = 0b010 ++# x
        go s x = cycle' s (IS (mkcall x) u u)
        u = errorX "must not be used in this test"
        test p l = \x (Fetch s) -> p $ go s x ^. l
    -- Black-box (outputs-based) properties
    it "doesn't write Memory" $ property $ test isNothing (_1 . osMWrite)
    it "doesn't write Data" $ property $ test isNothing (_1 . osDOp . _2)

    it "pushes PC to R" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osROp == (s ^. msRPtr + 1, Just (s ^. msPC + 1))

    -- White-box (microarchitectural) properties
    let stdelta f l = \x (Fetch s) -> (go s x ^. _2 . l) == f (s ^. l)

    it "doesn't set LF" $ property $ stdelta id msLoadFlag
    it "preserves DPtr" $ property $ stdelta id msDPtr
    it "increments RPtr" $ property $ stdelta (+1) msRPtr
    it "preserves T" $ property $ stdelta id msT
    it "advances PC" $ property $ \x (Fetch s) ->
      go s x ^. _2 . msPC == zeroExtend x
    it "fetches next" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osMRead == Just (zeroExtend x)
    it "addresses D" $ property $ \x (Fetch s) ->
      go s x ^. _1 . osDOp . _1 == s ^. msDPtr

  context "ALU" $ do
    let mkalu :: BitVector 13 -> BitVector 16
        mkalu x = 0b011 ++# x
        go s x d r = cycle' s (IS (mkalu x) d r)
    it "PC <- I[12] ? R : PC+1" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _2 . msPC ==
        case slice d12 d12 x of
          0 -> s ^. msPC + 1
          1 -> r

    it "I[7]: N <- T" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _1 . osDOp . _2 ==
        case slice d7 d7 x of
          0 -> Nothing
          1 -> Just (s ^. msT)

    it "I[6]: T <- R" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _1 . osROp . _2 ==
        case slice d6 d6 x of
          0 -> Nothing
          1 -> Just (s ^. msT)

    it "I[5]: [T] <- N" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _1 . osMWrite ==
        case slice d5 d5 x of
          0 -> Nothing
          1 -> Just (s ^. msT, d)

    context "I[4]: begin load" $ do
      it "triggers read of [T]" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _1 . osMRead == Just
          (case slice d4 d4 x of
             0 -> s ^. msPC + 1
             1 -> s ^. msT)
      it "sets load flag" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _2 . msLoadFlag == (slice d4 d4 x /= 0)

    context "I[3:2]: RPtr adjust" $ do
      it "updates RPtr" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _2 . msRPtr == s ^. msRPtr + signExtend (slice d3 d2 x)
      it "addresses R" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _1 . osROp . _1 ==
          s ^. msRPtr + signExtend (slice d3 d2 x)
    
    context "I[3:2]: DPtr adjust" $ do
      it "updates DPtr" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _2 . msDPtr == s ^. msDPtr + signExtend (slice d1 d0 x)
      it "addresses D" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _1 . osDOp . _1 ==
          s ^. msDPtr + signExtend (slice d1 d0 x)

    context "T'" $ do
      let mkalu' :: BitVector 4 -> BitVector 9 -> BitVector 16
          mkalu' op rest = 0b011 ++# slice d8 d8 rest ++# op ++#
                           slice d7 d0 rest
          go' op s x d r = cycle' s (IS (mkalu' op x) d r)
          t' op f = property $ \(Fetch s) x d r ->
                      go' op s x d r ^. _2 . msT == f s d r
          t'n op f = t' op $ \s n _ -> (s ^. msT) `f` n

      it "T" $ t' 0 $ \s _ _ -> s ^. msT
      it "N" $ t' 1 $ \_ n _ -> n
      it "T + N" $ t'n 2 (+)
      it "T & N" $ t'n 3 (.&.)
      it "T | N" $ t'n 4 (.|.)
      it "T | N" $ t'n 5 xor
      it "~T" $ t' 6 $ \s _ _ -> complement (s ^. msT)
      it "T == N" $ t'n 7 $ \t n -> pack $ repeat $ t == n
      it "N < T" $ t'n 8 $
        \t n -> pack $ repeat $ unpack @(Signed 16) n < unpack t
      it "N >> T" $ t'n 9 $ \t n -> n `shiftR` fromIntegral t
      it "T - 1" $ t' 10 $ \s _ _ -> s ^. msT - 1
      it "R" $ t' 11 $ \_ _ r -> r
      -- 12 reserved
      it "N << T" $ t'n 13 $ \t n -> n `shiftL` fromIntegral t
      it "depth" $ t' 14 $ \s _ _ -> zeroExtend (s ^. msDPtr)
      it "N < T" $ t'n 15 $ \t n -> pack $ repeat $ n < t


