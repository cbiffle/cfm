{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module TestUtil where

import Clash.Prelude

import Data.Maybe (isNothing)
import Control.Lens
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import Types
import CoreInterface

newtype Fetch = Fetch MS deriving (Show)

instance Arbitrary Fetch where
  arbitrary = Fetch <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> pure False)

newtype Load = Load MS deriving (Show)

instance Arbitrary Load where
  arbitrary = Load <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> pure True)

genspec :: (MS -> IS -> (MS, OS)) -> Spec
genspec sf = do
  context "reset" $ do
    let u = errorX "inputs undefined at reset"
        inputs = IS u u u
    it "does not write" $ property $
      isNothing $ sf def inputs ^. _2 . osMWrite
    it "fetches first instruction" $ property $
      sf def inputs ^. _2 . osMRead == 0
    it "starts out stack at 0" $ property $
      sf def inputs ^. _2 . osDOp == (0, 0, Nothing)
    it "starts out return stack at 0" $ property $
      sf def inputs ^. _2 . osROp == (0, 0, Nothing)

  context "load flag behavior" $ do
    let u = errorX "must not be used"
        go s x = sf s (IS x u u)
        test p l = \(Load s) -> p $ go s u ^. l
    it "doesn't write Memory" $ property $ test isNothing (_2 . osMWrite)
    it "doesn't write Return" $ property $ test isNothing (_2 . osROp . _3)
    it "doesn't write Data" $ property $ test isNothing (_2 . osDOp . _3)
    it "fetches current" $ property $ \(Load s) ->
      go s u ^. _2 . osMRead == s ^. msPC
    it "addresses D" $ property $ \(Load s) ->
      go s u ^. _2 . osDOp . _1 == s ^. msDPtr
    it "addresses R" $ property $ \(Load s) ->
      go s u ^. _2 . osROp . _1 == s ^. msRPtr

    let stdelta f l = \(Load s) -> (go s u ^. _1 . l) == f (s ^. l)

    it "clears load flag" $ property $ stdelta (const False) msLoadFlag
    it "preserves DPtr" $ property $ stdelta id msDPtr
    it "preserves RPtr" $ property $ stdelta id msRPtr
    it "preserves PC" $ property $ stdelta id msPC

    it "loads T" $ property $ \(Load s) v ->
      go s v ^. _1 . msT == v

  context "universal instruction properties" $ do
    let go s x d r = sf s (IS x d r)
        u = errorX "must not be used in this test"

    it "addresses R using new RPtr" $ property $ \x (Fetch s) ->
      let (s', o) = go s x u u
      in o ^. osROp . _1 == s' ^. msRPtr
    it "addresses D using new DPtr" $ property $ \x (Fetch s) ->
      let (s', o) = go s x u u
      in o ^. osDOp . _1 == s' ^. msDPtr
    it "always produces a load or fetch" $ property $ \x (Fetch s) d r ->
      let (s', o) = go s x d r
      in o ^. osMRead == if s' ^. msLoadFlag
                           then s ^. msT & slice d15 d1
                           else s' ^. msPC

  context "literal push" $ do
    let mklit :: BitVector 15 -> BitVector 16
        mklit x = 1 ++# x
        go s x = sf s (IS (mklit x) u u)
        u = errorX "must not be used in this test"

    instDoesNotWriteM mklit
    instDoesNotWriteR mklit
    instPreservesRPtr mklit
    instLeavesLFClear mklit
    instFetches mklit
    it "increments DPtr" $ instChanges mklit msDPtr (+1)
    it "advances PC" $ instChanges mklit msPC (+1)

    it "flushes T" $ property $ \x (Fetch s) ->
      go s x ^. _2 . osDOp == (s ^. msDPtr + 1, 1, Just (s ^. msT))

    it "loads T from literal" $ property $ \x (Fetch s) ->
      go s x ^. _1 . msT == zeroExtend x

  context "jump" $ do
    let mkjmp :: BitVector 13 -> BitVector 16
        mkjmp x = 0 ++# x

    instDoesNotWriteM mkjmp
    instDoesNotWriteR mkjmp
    instDoesNotWriteD mkjmp
    instLeavesLFClear mkjmp
    instPreservesDPtr mkjmp
    instPreservesRPtr mkjmp
    instFetches mkjmp

    it "preserves T" $ instPreserves mkjmp msT

    let go s x = sf s (IS (mkjmp x) u u)
        u = errorX "must not be used in this test"

    it "always jumps" $ property $ \x (Fetch s) ->
      go s x ^. _1 . msPC == zeroExtend x

  context "conditional jump" $ do
    let mkjmp :: BitVector 13 -> BitVector 16
        mkjmp x = 0b001 ++# x
    instDoesNotWriteM mkjmp
    instDoesNotWriteR mkjmp
    instDoesNotWriteD mkjmp
    instLeavesLFClear mkjmp
    instPreservesRPtr mkjmp
    instFetches mkjmp

    it "decrements DPtr" $ instChanges mkjmp msDPtr (+ 0xFFFF)

    let go s x d = sf s (IS (mkjmp x) d u)
        u = errorX "must not be used in this test"
        stdelta f l = \x (Fetch s) d -> (go s x d ^. _1 . l) == f (s ^. l)

    it "loads T from N" $ property $ \x (Fetch s) d ->
      go s x d ^. _1 . msT == d

    it "jumps when T==0, otherwise proceeds" $ property $ \x (Fetch s) d ->
      go s x d ^. _1 . msPC == if s ^. msT == 0
                                 then zeroExtend x
                                 else s ^. msPC + 1

  context "call" $ do
    let mkcall :: BitVector 13 -> BitVector 16
        mkcall x = 0b010 ++# x
    instDoesNotWriteM mkcall
    instDoesNotWriteD mkcall
    instLeavesLFClear mkcall
    instPreservesDPtr mkcall
    instFetches mkcall

    it "increments RPtr" $ instChanges mkcall msRPtr (+1)
    it "preserves T" $ instPreserves mkcall msT

    let go s x = sf s (IS (mkcall x) u u)
        u = errorX "must not be used in this test"
        test p l = \x (Fetch s) -> p $ go s x ^. l
        stdelta f l = \x (Fetch s) -> (go s x ^. _1 . l) == f (s ^. l)

    it "pushes return PC to R as byte address" $ property $ \x (Fetch s) ->
      go s x ^. _2 . osROp . _3 == Just ((s ^. msPC + 1) ++# 0)

    it "always jumps" $ property $ \x (Fetch s) ->
      go s x ^. _1 . msPC == zeroExtend x

  context "ALU" $ do
    let mkalu :: BitVector 13 -> BitVector 16
        mkalu x = 0b011 ++# x
        go s x d r = sf s (IS (mkalu x) d r)

    it "PC <- I[12] ? R : PC+1" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _1 . msPC ==
        case slice d12 d12 x of
          0 -> s ^. msPC + 1
          1 -> slice d15 d1 r

    it "I[7]: N <- T" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _2 . osDOp . _3 ==
        case slice d7 d7 x of
          0 -> Nothing
          1 -> Just (s ^. msT)

    it "I[6]: R <- T" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _2 . osROp . _3 ==
        case slice d6 d6 x of
          0 -> Nothing
          1 -> Just (s ^. msT)

    it "I[5]: [T] <- N" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _2 . osMWrite ==
        case slice d5 d5 x of
          0 -> Nothing
          1 -> Just (slice d15 d1 (s ^. msT), d)

    context "I[3:2]: RPtr adjust" $ do
      it "updates RPtr" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _1 . msRPtr == s ^. msRPtr + signExtend (slice d3 d2 x)
      it "addresses R" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _2 . osROp . _1 ==
          s ^. msRPtr + signExtend (slice d3 d2 x)

    context "I[3:2]: DPtr adjust" $ do
      it "updates DPtr" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _1 . msDPtr == s ^. msDPtr + signExtend (slice d1 d0 x)
      it "addresses D" $ property $ \(Fetch s) x d r ->
        go s x d r ^. _2 . osDOp . _1 ==
          s ^. msDPtr + signExtend (slice d1 d0 x)

    context "T'" $ do
      let mkalu' :: BitVector 4 -> BitVector 9 -> BitVector 16
          mkalu' op rest = 0b011 ++# slice d8 d8 rest ++# op ++#
                           slice d7 d0 rest
          go' op s x d r = sf s (IS (mkalu' op x) d r)
          t' op f = property $ \(Fetch s) x d r ->
                      go' op s x d r ^. _1 . msT == f s d r
          t'n op f = t' op $ \s n _ -> (s ^. msT) `f` n

      it "T"      $ t'   0 $ \s _ _ -> s ^. msT
      it "N"      $ t'   1 $ \_ n _ -> n
      it "T + N"  $ t'n  2 (+)
      it "T & N"  $ t'n  3 (.&.)
      it "T | N"  $ t'n  4 (.|.)
      it "T | N"  $ t'n  5 xor
      it "~T"     $ t'   6 $ \s _ _ -> complement (s ^. msT)
      it "T == N" $ t'n  7 $ \t n -> pack $ repeat $ t == n
      it "N < T"  $ t'n  8 $
        \t n -> pack $ repeat $ unpack @(Signed 16) n < unpack t
      it "N >> T" $ t'n  9 $ \t n -> n `shiftR` fromIntegral (slice d3 d0 t)
      it "N - T"  $ t'n 10 $ flip (-)
      it "R"      $ t'  11 $ \_ _ r -> r

      context "[T]" $ do
        it "triggers read of [T]" $ property $ \(Fetch s) x d r ->
          go' 12 s x d r ^. _2 . osMRead == slice d15 d1 (s ^. msT)
        it "sets load flag" $ property $ \(Fetch s) x d r ->
          go' 12 s x d r ^. _1 . msLoadFlag == True

      it "N << T" $ t'n 13 $ \t n -> n `shiftL` fromIntegral (slice d3 d0 t)
      it "depth"  $ t'  14 $ \s _ _ -> zeroExtend (s ^. msDPtr)
      it "N U< T"  $ t'n 15 $ \t n -> pack $ repeat $ n < t

  where
  instDoesNotWriteM mkinst = it "does not write memory" $ property $
    \(Fetch s) x -> isNothing $ sf s (IS (mkinst x) u u) ^. _2 . osMWrite
      where u = errorX "must be unused"

  instDoesNotWriteR mkinst = it "does not write return" $ property $
    \(Fetch s) x -> isNothing $ sf s (IS (mkinst x) u u) ^. _2 . osROp . _3
      where u = errorX "must be unused"
  
  instDoesNotWriteD mkinst = it "does not write return" $ property $
    \(Fetch s) x -> isNothing $ sf s (IS (mkinst x) u u) ^. _2 . osDOp . _3
      where u = errorX "must be unused"
  
  instLeavesLFClear mkinst = it "leaves load flag clear" $ property $
    \(Fetch s) x -> not $ sf s (IS (mkinst x) u u) ^. _1 . msLoadFlag
      where u = errorX "must be unused"
  
  instChanges mkinst l f = property $
    \(Fetch s) x -> sf s (IS (mkinst x) u u) ^. _1 . l == f (s ^. l)
      where u = errorX "must be unused"
  
  instPreserves mkinst l = instChanges mkinst l id
  
  instPreservesDPtr mkinst = it "preserves DPtr" $ instPreserves mkinst msDPtr
  instPreservesRPtr mkinst = it "preserves RPtr" $ instPreserves mkinst msRPtr
  
  -- Distinguishes an instruction fetch from a load.
  instFetches mkinst = it "fetches" $ property $
    \(Fetch s) x ->
      let (s', o) = sf s (IS (mkinst x) u u)
      in o ^. osMRead == s' ^. msPC
      where u = errorX "must be unused"
 
