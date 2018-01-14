{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module RTL.TestUtil where

import Clash.Prelude

import Data.Maybe (isNothing)
import Control.Lens hiding (op)
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import CFM.Inst
import RTL.CoreInterface

newtype Fetch = Fetch MS deriving (Show)

instance Arbitrary Fetch where
  arbitrary = Fetch <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> pure BusFetch <*> arbitrary)

newtype Load = Load MS deriving (Show)

instance Arbitrary Load where
  arbitrary = Load <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> pure (BusData True) <*> arbitrary)

isWrite :: BusReq -> Bool
isWrite (Just (_, Just _)) = True
isWrite _ = False

genspec :: (MS -> IS -> (MS, OS)) -> Spec
genspec sf = do
  context "reset" $ do
    let u = errorX "inputs undefined at reset"
        inputs = IS u u u u
    it "fetches first instruction from memory, no write" $ property $
      sf def inputs ^. _2 . osMReq `shouldBe` Just (0, Nothing)
    it "starts out stack at 0" $ property $
      sf def inputs ^. _2 . osDOp == (0, 0, Nothing)
    it "starts out return stack at 0" $ property $
      sf def inputs ^. _2 . osROp == (0, 0, Nothing)

  context "load flag behavior" $ do
    let u = errorX "must not be used"
        go s x y = sf s (IS x y u u)
        test p l = \(Load s) -> p $ go s u u ^. l
    it "doesn't write Return" $ property $ test isNothing (_2 . osROp . _3)
    it "doesn't write Data" $ property $ test isNothing (_2 . osDOp . _3)
    it "fetches current" $ property $ \(Load s) ->
      go s u u ^. _2 . osMReq `shouldBe` Just (zeroExtend (s ^. msPC), Nothing)
    it "asserts fetch" $ property $ \(Load s) ->
      go s u u ^. _2 . osFetch `shouldBe` True
    it "addresses D" $ property $ \(Load s) ->
      go s u u ^. _2 . osDOp . _1 == s ^. msDPtr
    it "addresses R" $ property $ \(Load s) ->
      go s u u ^. _2 . osROp . _1 == s ^. msRPtr

    let stdelta f l = \(Load s) -> (go s u u ^. _1 . l) == f (s ^. l)

    it "returns to fetch state" $ property $ stdelta (const BusFetch) msBusState
    it "preserves DPtr" $ property $ stdelta id msDPtr
    it "preserves RPtr" $ property $ stdelta id msRPtr
    it "preserves PC" $ property $ stdelta id msPC

    it "loads T" $ property $ \(Load s) v ->
      go s v v ^. _1 . msT == v

  context "universal instruction properties" $ do
    let go s x d r = sf s (IS x x d r)
        u = errorX "must not be used in this test"

    it "addresses R using new RPtr" $ property $ \x (Fetch s) ->
      let (s', o) = go s x u u
      in o ^. osROp . _1 == s' ^. msRPtr
    it "addresses D using new DPtr" $ property $ \x (Fetch s) ->
      let (s', o) = go s x u u
      in o ^. osDOp . _1 == s' ^. msDPtr

  context "literal push" $ do
    let mklit :: BitVector 15 -> BitVector 16
        mklit x = 1 ++# x
        go s x = sf s (IS (mklit x) u u u)
        u = errorX "must not be used in this test"

    instDoesNotWriteM mklit
    instDoesNotWriteR mklit
    instPreservesRPtr mklit
    instRemainsInFetchState mklit
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
    instRemainsInFetchState mkjmp
    instPreservesDPtr mkjmp
    instPreservesRPtr mkjmp
    instFetches mkjmp

    it "preserves T" $ instPreserves mkjmp msT

    let go s x = sf s (IS (mkjmp x) u u u)
        u = errorX "must not be used in this test"

    it "always jumps" $ property $ \x (Fetch s) ->
      go s x ^. _1 . msPC == zeroExtend x

  context "conditional jump" $ do
    let mkjmp :: BitVector 13 -> BitVector 16
        mkjmp x = 0b001 ++# x
    instDoesNotWriteM mkjmp
    instDoesNotWriteR mkjmp
    instDoesNotWriteD mkjmp
    instRemainsInFetchState mkjmp
    instPreservesRPtr mkjmp
    instFetches mkjmp

    it "decrements DPtr" $ instChanges mkjmp msDPtr (+ 0xFFFF)

    let go s x d = sf s (IS (mkjmp x) u d u)
        u = errorX "must not be used in this test"

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
    instRemainsInFetchState mkcall
    instPreservesDPtr mkcall
    instFetches mkcall

    it "increments RPtr" $ instChanges mkcall msRPtr (+1)
    it "preserves T" $ instPreserves mkcall msT

    let go s x = sf s (IS (mkcall x) u u u)
        u = errorX "must not be used in this test"

    it "pushes return PC to R as byte address" $ property $ \x (Fetch s) ->
      go s x ^. _2 . osROp . _3 == Just (low ++# (s ^. msPC + 1) ++# low)

    it "always jumps" $ property $ \x (Fetch s) ->
      go s x ^. _1 . msPC == zeroExtend x

  context "ALU" $ do
    let mkalu :: BitVector 13 -> BitVector 16
        mkalu x = 0b011 ++# x
        go s x d r = sf s (IS (mkalu x) u d r)
        u = errorX "must not be used in this test"

    it "PC <- I[12] ? R[14:1] : PC+1" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _1 . msPC ==
        case slice d12 d12 x of
          0 -> s ^. msPC + 1
          _ -> slice d14 d1 r

    it "I[7]: N <- T" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _2 . osDOp . _3 ==
        case slice d7 d7 x of
          0 -> Nothing
          _ -> Just (s ^. msT)

    it "I[6]: R <- T" $ property $ \(Fetch s) x d r ->
      go s x d r ^. _2 . osROp . _3 ==
        case slice d6 d6 x of
          0 -> Nothing
          _ -> Just (s ^. msT)

    context "I[5]: [T] <- N" $ do
      it "does not write memory when clear" $ property $ \(Fetch s) x d r ->
        slice d5 d5 x == 0 ==>
          isWrite (go s x d r ^. _2 . osMReq) `shouldBe` False
      it "does not write I/O when clear" $ property $ \(Fetch s) x d r ->
        slice d5 d5 x == 0 ==>
          isWrite (go s x d r ^. _2 . osIReq) `shouldBe` False
      it "writes I/O or memory when set" $ property $ \(Fetch s) x d r ->
        slice d11 d8 x /= 12 ==> -- ignore simultaneous loads/stores
        slice d5 d5 x == 1 ==> do
          let result = go s x d r ^. _2
              space = unpack $ slice d4 d4 x
          result ^. osMReq `shouldBe` case space of
            ISpace -> Nothing
            MSpace -> Just (slice d15 d1 (s ^. msT), Just d)
          result ^. osIReq `shouldBe` case space of
            MSpace -> Nothing
            ISpace -> Just (slice d15 d1 (s ^. msT), Just d)

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
          go' op s x d r = sf s (IS (mkalu' op x) (errorX "IO") d r)
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
          (slice d5 d5 x == 0) ==> do -- ignore simultaneous loads/stores
            let result = go' 12 s x d r ^. _2
                space = unpack $ slice d4 d4 x

            result ^. osMReq `shouldBe` case space of
                ISpace -> Nothing
                MSpace -> Just (slice d15 d1 (s ^. msT), Nothing)
            result ^. osIReq `shouldBe` case space of
                ISpace -> Just (slice d15 d1 (s ^. msT), Nothing)
                MSpace -> Nothing
        it "enters load state" $ property $ \(Fetch s) x d r ->
          go' 12 s x d r ^. _1 . msBusState == BusData True
        it "does not assert fetch" $ property $ \(Fetch s) x d r ->
          go' 12 s x d r ^. _2 . osFetch `shouldBe` False

      it "N << T" $ t'n 13 $ \t n -> n `shiftL` fromIntegral (slice d3 d0 t)
      it "depth"  $ t'  14 $ \s _ _ -> (s ^. msRPtr) ++# (s ^. msDPtr)
      it "N U< T"  $ t'n 15 $ \t n -> pack $ repeat $ n < t

  where
  instDoesNotWriteM mkinst = it "does not write memory" $ property $
    \(Fetch s) x -> not $ isWrite $ sf s (IS (mkinst x) u u u) ^. _2 . osMReq
      where u = errorX "must be unused"

  instDoesNotWriteR mkinst = it "does not write return" $ property $
    \(Fetch s) x -> isNothing $ sf s (IS (mkinst x) u u u) ^. _2 . osROp . _3
      where u = errorX "must be unused"
  
  instDoesNotWriteD mkinst = it "does not write return" $ property $
    \(Fetch s) x -> isNothing $ sf s (IS (mkinst x) u u u) ^. _2 . osDOp . _3
      where u = errorX "must be unused"
  
  instRemainsInFetchState mkinst = it "remains in fetch state" $ property $
    \(Fetch s) x ->
      sf s (IS (mkinst x) u u u) ^. _1 . msBusState `shouldBe` BusFetch
      where u = errorX "must be unused"
  
  instChanges mkinst l f = property $
    \(Fetch s) x -> sf s (IS (mkinst x) u u u) ^. _1 . l == f (s ^. l)
      where u = errorX "must be unused"
  
  instPreserves mkinst l = instChanges mkinst l id
  
  instPreservesDPtr mkinst = it "preserves DPtr" $ instPreserves mkinst msDPtr
  instPreservesRPtr mkinst = it "preserves RPtr" $ instPreserves mkinst msRPtr
  
  -- Distinguishes an instruction fetch from a load.
  instFetches mkinst = context "fetches" $ do
    it "right address" $ property $
      \(Fetch s) x ->
        let (s', o) = sf s (IS (mkinst x) u u u)
        in o ^. osMReq `shouldBe` Just (zeroExtend (s' ^. msPC), Nothing)
    it "asserts fetch" $ property $
      \(Fetch s) x ->
        let (_, o) = sf s (IS (mkinst x) u u u)
        in o ^. osFetch `shouldBe` True
    where u = errorX "must be unused"
 
