{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A target that runs the actual processor RTL in an isolated context.
module Target.RTL where

import qualified Clash.Signal as C
import Clash.Sized.Vector (Vec(..))
import GHC.TypeLits (KnownNat)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent.Chan

import CFM.Types
import Target
import Target.Stub (debugStub)
import Target.H2T
import qualified RTL.TargetTop as R

-- We'll use a 'Chan' to incrementally create a lazy list of stimuli, and stash
-- that in the environment as a Reader.
--
-- We'll then apply the circuit to it and stash its unconsumed output as State.

data IORTLS = IORTLS
  { iortlsInp :: [(Maybe Cell, Bool)]
  , iortlsCnt :: Int
  } deriving (Show)

-- | RTL H2T wrapper monad
newtype IORTL' x = IORTL' (ReaderT (Chan (Maybe Cell, Bool))
                          (StateT IORTLS IO) x)
        deriving (Functor, Applicative, Monad,
                  MonadReader (Chan (Maybe Cell, Bool)),
                  MonadState IORTLS,
                  MonadIO)

instance MonadH2T IORTL' where
  tick w b = do
    c <- ask
    liftIO $ writeChan c (w, b)
    r : rest <- gets iortlsInp
    modify $ \s -> s { iortlsInp = rest, iortlsCnt = iortlsCnt s + 1 }
    pure r

  cycles = gets iortlsCnt

runIORTL' :: IORTL' x -> IO x
runIORTL' (IORTL' a) = do
  stim <- newChan
  stimLazy <- getChanContents stim
  let s = IORTLS { iortlsInp = target debugStub stimLazy
                 , iortlsCnt = 0
                 }
  evalStateT (runReaderT a stim) s

newtype IORTL x = IORTL (H2T IORTL' x)
  deriving (Functor, Applicative, Monad, MonadTarget, MonadIO)

instance MonadH2T IORTL where
  tick w b = IORTL $ H2T $ tick w b
  cycles = IORTL $ H2T cycles


runIORTL :: IORTL x -> IO x
runIORTL (IORTL a) = runIORTL' (runH2T a)

target :: (KnownNat n)
       => Vec n Cell
       -> [(Maybe Cell, Bool)]
       -> [(Maybe Cell, Bool)]
target img = C.simulateB_lazy $
             R.targetTop img C.systemClockGen C.systemResetGen
