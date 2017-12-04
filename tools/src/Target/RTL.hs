{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A target that runs the actual processor RTL in an isolated context.
module Target.RTL where

import Prelude hiding (Word)
import qualified Clash.Signal as C
import Clash.Sized.Vector (Vec(..))
import Clash.Sized.BitVector ((++#))
import qualified Clash.Sized.Vector as V
import GHC.TypeLits (KnownNat)
import Data.List (foldl', find)
import Data.Maybe (isJust, fromJust)
import Data.Functor.Identity
import Control.Monad (join)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

import CFM.Types
import Target
import qualified RTL.TargetTop as R

newtype RTL x = RTL (Coroutine (Request (Maybe Word, Bool) (Maybe Word, Bool))
                               Identity
                               x)
                deriving (Functor, Applicative, Monad)

-- | Advances the clock, providing inputs and returning outputs.
tick :: (Maybe Word, Bool) -> RTL (Maybe Word, Bool)
tick x = RTL (request x)

-- | Transmits a word to the target system via the H2T buffer.
put :: Word -> RTL ()
put w = do
  _ <- tick (Just w, False)
  awaitTake

-- | Blocks until the H2T buffer is empty.
awaitTake :: RTL ()
awaitTake = do
  (_, te) <- tick (Nothing, False)
  if te
    then pure ()
    else awaitTake

-- | Receives a word from the target via the T2H buffer, spinning until one is
-- available.
get :: RTL Word
get = do
  (t2h, _) <- tick (Nothing, True)
  case t2h of
    Just x -> pure x
    Nothing -> get

-- | Implementation factor of debug protocol commands: checks for the
-- conventional 0 success code, and executes @action@ only if the command
-- succeeded.
checkResponse :: RTL x -> RTL x
checkResponse action = do
  r <- get
  if r == 0
    then action
    else error "target command failed"

instance MonadTarget RTL where
  tload addr = do
    put 0
    put (addr ++# 0)
    checkResponse get

  tstore addr value = do
    put 1
    put value
    put (addr ++# 0)
    checkResponse (pure ())

  tpush value = do
    put 2
    put value
    checkResponse (pure ())

  tpop = do
    put 3
    checkResponse get

  tpushR value = do
    put 4
    put value
    checkResponse (pure ())

  tpopR = do
    put 5
    checkResponse get

  tcall addr = do
    put 6
    put (addr ++# 0)
    checkResponse (pure ())

-- | Runs an 'RTL' action against a fresh instance of the system with the debug
-- stub loaded.
sim :: RTL x -> x
sim (RTL c) = fromJust $ join $ find isJust resp
  where
    t2h = target debugStub h2t
    (h2t, resp) = unzip $ go c t2h

    go c ~(x : xs) = case runIdentity (resume c) of
        Right result -> [((Nothing, False), Just result)]
        Left (Request out f) -> (out, Nothing) : go (f x) xs

target :: (KnownNat n)
       => Vec n Word
       -> [(Maybe Word, Bool)]
       -> [(Maybe Word, Bool)]
target img = C.simulateB_lazy $ R.targetTop img C.systemClockGen C.systemResetGen

debugStub :: Vec 8192 Word
debugStub = V.replace (0 :: Int) 0x1f56 $
            foldl' (\v (a, i) -> V.replace a i v) (V.repeat 0xDEAD) $
            zip [0x3E00 `div` 2 :: Int ..]
                [ 0x6147
                , 0x700c
                , 0xfffb
                , 0x6600
                , 0x6c00
                , 0x3f02
                , 0xfff9
                , 0x6600
                , 0x6123
                , 0x710f
                , 0xffff
                , 0x6600
                , 0x6c00
                , 0x3f0a
                , 0xfffd
                , 0x6600
                , 0x7c0c
                , 0x5f0a
                , 0x8000
                , 0x6700
                , 0x3f1c
                , 0x6103
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x6c00
                , 0x5f02
                , 0x5f11
                , 0x8001
                , 0x6700
                , 0x3f27
                , 0x6103
                , 0x5f0a
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x6123
                , 0x6103
                , 0x5f11
                , 0x8002
                , 0x6700
                , 0x3f2f
                , 0x6103
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x5f11
                , 0x8003
                , 0x6700
                , 0x3f37
                , 0x6103
                , 0x8000
                , 0x5f02
                , 0x5f02
                , 0x5f11
                , 0x8004
                , 0x6700
                , 0x3f40
                , 0x6103
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x6147
                , 0x5f11
                , 0x8005
                , 0x6700
                , 0x3f49
                , 0x6103
                , 0x8000
                , 0x5f02
                , 0x6b8d
                , 0x5f02
                , 0x5f11
                , 0x8006
                , 0x6700
                , 0x3f52
                , 0x6103
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x5f00
                , 0x5f11
                , 0x6103
                , 0x8001
                , 0x5f02
                , 0x1f11
                , 0xA152
                , 0x6600
                , 0x8000
                , 0x6123
                , 0x6103
                , 0x1f11
                ]
