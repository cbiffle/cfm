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
import Data.List (foldl')
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent.Chan

import CFM.Types
import Target
import qualified RTL.TargetTop as R

-- We'll use a 'Chan' to incrementally create a lazy list of stimuli, and stash
-- that in the environment as a Reader.
--
-- We'll then apply the circuit to it and stash its unconsumed output as State.

-- | IO monad wrapper for interacting with the RTL target while performing I/O.
newtype IORTL x = IORTL (ReaderT (Chan (Maybe Word, Bool))
                        (StateT [(Maybe Word, Bool)] IO) x)
        deriving (Functor, Applicative, Monad,
                  MonadReader (Chan (Maybe Word, Bool)),
                  MonadState [(Maybe Word, Bool)],
                  MonadIO)

-- | Advances the simulation one cycle, providing host-to-target signals, and
-- returns the target-to-host response.
tick :: Maybe Word -> Bool -> IORTL (Maybe Word, Bool)
tick w b = do
  c <- ask
  liftIO $ writeChan c (w, b)
  r : rest <- get
  put rest
  pure r

-- | Initializes a new target and runs an 'IORTL' action.
runIORTL :: IORTL x -> IO x
runIORTL (IORTL a) = do
  stim <- newChan
  stimLazy <- getChanContents stim
  let resp = target debugStub stimLazy
  evalStateT (runReaderT a stim) resp

-- | Transmits a word to the target system via the H2T buffer.
tput :: Word -> IORTL ()
tput w = do
  _ <- tick (Just w) False
  awaitTake

-- | Blocks until the H2T buffer is empty.
awaitTake :: IORTL ()
awaitTake = do
  (_, te) <- tick Nothing False
  if te
    then pure ()
    else awaitTake

-- | Receives a word from the target via the T2H buffer, spinning until one is
-- available.
tget :: IORTL Word
tget = do
  (t2h, _) <- tick Nothing True
  case t2h of
    Just x -> pure x
    Nothing -> tget

-- | Implementation factor of debug protocol commands: checks for the
-- conventional 0 success code, and executes @action@ only if the command
-- succeeded.
checkResponse :: IORTL x -> IORTL x
checkResponse action = do
  r <- tget
  if r == 0
    then action
    else error "target command failed"

instance MonadTarget IORTL where
  tload addr = do
    tput 0
    tput (addr ++# 0)
    checkResponse tget

  tstore addr value = do
    tput 1
    tput value
    tput (addr ++# 0)
    checkResponse (pure ())

  tpush value = do
    tput 2
    tput value
    checkResponse (pure ())

  tpop = do
    tput 3
    checkResponse tget

  tpushR value = do
    tput 4
    tput value
    checkResponse (pure ())

  tpopR = do
    tput 5
    checkResponse tget

  tcall addr = do
    tput 6
    tput (addr ++# 0)
    -- Calls expect *two* responses, before and after execution.
    checkResponse $ checkResponse $ pure ()


--------------------------------------------------------------------------
-- The actual target and implementation of the debug protocol.

target :: (KnownNat n)
       => Vec n Word
       -> [(Maybe Word, Bool)]
       -> [(Maybe Word, Bool)]
target img = C.simulateB_lazy $
             R.targetTop img C.systemClockGen C.systemResetGen

debugStub :: Vec 8192 Word
debugStub = V.replace (0 :: Int) 0x1f58 $
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
                , 0x1f11
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
                , 0x1f11
                , 0x8002
                , 0x6700
                , 0x3f2f
                , 0x6103
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x1f11
                , 0x8003
                , 0x6700
                , 0x3f37
                , 0x6103
                , 0x8000
                , 0x5f02
                , 0x5f02
                , 0x1f11
                , 0x8004
                , 0x6700
                , 0x3f40
                , 0x6103
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x6147
                , 0x1f11
                , 0x8005
                , 0x6700
                , 0x3f49
                , 0x6103
                , 0x8000
                , 0x5f02
                , 0x6b8d
                , 0x5f02
                , 0x1f11
                , 0x8006
                , 0x6700
                , 0x3f54
                , 0x6103
                , 0x5f0a
                , 0x8000
                , 0x5f02
                , 0x5f00
                , 0x8000
                , 0x5f02
                , 0x1f11
                , 0x6103
                , 0x8001
                , 0x5f02
                , 0x1f11
                , 0x8000
                , 0x8000
                , 0x6123
                , 0x6103
                , 0x1f11
                ]
