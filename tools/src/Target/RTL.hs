{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
-- | A target that runs the actual processor RTL in an isolated context.
module Target.RTL where

import qualified Clash.Signal as C
import Clash.Sized.Vector (Vec(..))
import Clash.Sized.BitVector ((++#))
import GHC.TypeLits (KnownNat)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent.Chan

import CFM.Types
import Target
import Target.Stub (debugStub)
import qualified RTL.TargetTop as R

-- We'll use a 'Chan' to incrementally create a lazy list of stimuli, and stash
-- that in the environment as a Reader.
--
-- We'll then apply the circuit to it and stash its unconsumed output as State.

data IORTLS = IORTLS
  { iortlsInp :: [(Maybe Cell, Bool)]
  , iortlsCnt :: Int
  } deriving (Show)

-- | IO monad wrapper for interacting with the RTL target while performing I/O.
newtype IORTL x = IORTL (ReaderT (Chan (Maybe Cell, Bool))
                        (StateT IORTLS IO) x)
        deriving (Functor, Applicative, Monad,
                  MonadReader (Chan (Maybe Cell, Bool)),
                  MonadState IORTLS,
                  MonadIO)

-- | Advances the simulation one cycle, providing host-to-target signals, and
-- returns the target-to-host response.
tick :: Maybe Cell -> Bool -> IORTL (Maybe Cell, Bool)
tick w b = do
  c <- ask
  liftIO $ writeChan c (w, b)
  r : rest <- gets iortlsInp
  modify $ \s -> s { iortlsInp = rest, iortlsCnt = iortlsCnt s + 1 }
  pure r

-- | Initializes a new target and runs an 'IORTL' action.
runIORTL :: IORTL x -> IO (x, Int)
runIORTL (IORTL a) = do
  stim <- newChan
  stimLazy <- getChanContents stim
  let s = IORTLS { iortlsInp = target debugStub stimLazy
                 , iortlsCnt = 0
                 }
  (x, s') <- runStateT (runReaderT a stim) s
  pure (x, iortlsCnt s')

-- | Transmits a word to the target system via the H2T buffer.
tput :: Cell -> IORTL ()
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
tget :: IORTL Cell
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
    else error $ "target command failed: " ++ show r

instance MonadTarget IORTL where
  tload addr = do
    tput 0
    tput (addr ++# 0)
    checkResponse tget

  tstore addr value = do
    tput 1
    tput value
    tput (addr ++# 0)

  tpush value = do
    tput 2
    tput value

  tpop = do
    tput 3
    checkResponse tget

  tcall addr = do
    tput 4
    tput (addr ++# 0)


--------------------------------------------------------------------------
-- The actual target and implementation of the debug protocol.

target :: (KnownNat n)
       => Vec n Cell
       -> [(Maybe Cell, Bool)]
       -> [(Maybe Cell, Bool)]
target img = C.simulateB_lazy $
             R.targetTop img C.systemClockGen C.systemResetGen
