{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Target.RTLSpec where

import Test.Hspec
import Test.QuickCheck
import Clash.Class.Resize (zeroExtend)
import Clash.Class.BitPack (pack)
import Control.Monad (zipWithM_)
import Control.Monad.IO.Class (liftIO)

import CFM.Types
import CFM.Inst
import Target
import Target.RTL

stubAddr :: WordAddr
stubAddr = 0x3E00 `div` 2

newtype AvoidStub = AvoidStub WordAddr deriving (Eq, Show)

instance Arbitrary AvoidStub where
  arbitrary = AvoidStub <$>
    ((zeroExtend <$> arbitrary @(_ 13)) `suchThat` (< stubAddr))

spec = do
  context "initialization state" $
    it "initializes the bottom of RAM to 0xDEAD" $ property $
      \(AvoidStub addr) -> runIORTL (tload addr) >>= (`shouldBe` 0xDEAD)

  it "remembers the effect of stores" $ property $
    \(AvoidStub addr) value ->
      runIORTL (tstore addr value >> tload addr) >>= (`shouldBe` value)

  context "popping what was pushed" $ do
    it "on the parameter stack" $ property $ \v ->
      runIORTL (tpush v >> tpop) >>= (`shouldBe` v)
    it "on the return stack" $ property $ \v ->
      runIORTL (tpushR v >> tpopR) >>= (`shouldBe` v)

  context "calling routines" $ do
    let assemble insts = zipWithM_ tstore [0..] $ map pack insts
        ret = NotLit $ ALU True T False False False (Res 0) (-1) 0

        guardedCall :: IORTL () -> IORTL x -> IORTL x
        guardedCall setup teardown = do
          tpush 0xBEEF
          tpushR 0xCAFE
          setup
          tcall 0
          r <- teardown
          pguard <- tpop
          rguard <- tpopR
          liftIO $ pguard `shouldBe` 0xBEEF
          liftIO $ rguard `shouldBe` 0xCAFE
          pure r

    it "can simply return" $ runIORTL $ do
      assemble [ ret ]
      guardedCall (pure ()) (pure ())

    it "can push and return" $ runIORTL $ do
      assemble [ Lit 0x7FFF
               , ret
               ]
      pure () `guardedCall` do
        r <- tpop
        liftIO $ r `shouldBe` 0x7FFF
