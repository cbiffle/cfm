{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Support for the H2T FIFO interface used for emulated targets.
module Target.H2T where

import Control.Monad.Trans
import CFM.Types
import Target

class Monad m => MonadH2T m where
  -- | Feeds the target a H2T word and a take flag, and pulls the target's T2H
  -- and empty flags in response. This advances the target by one cycle.
  tick :: Maybe Cell -> Bool -> m (Maybe Cell, Bool)
  -- | Reads the target's cycle counter. This may advance the cycle counter to
  -- do so.
  cycles :: m Int

-- | Transmits a word to the target system via the H2T buffer.
tput :: MonadH2T m => Cell -> m ()
tput w = do
  _ <- tick (Just w) False
  awaitTake

-- | Blocks until the H2T buffer is empty.
awaitTake :: MonadH2T m => m ()
awaitTake = do
  (_, te) <- tick Nothing False
  if te
    then pure ()
    else awaitTake

-- | Receives a word from the target via the T2H buffer, spinning until one is
-- available.
tget :: MonadH2T m => m Cell
tget = do
  (t2h, _) <- tick Nothing False
  case t2h of
    Just x -> tick Nothing True >> pure x
    Nothing -> tget

-- | Implementation factor of debug protocol commands: checks for the
-- conventional 0 success code, and executes @action@ only if the command
-- succeeded.
checkResponse :: MonadH2T m => m x -> m x
checkResponse action = do
  r <- tget
  if r == 0
    then action
    else error $ "target command failed: " ++ show r

newtype H2T m x = H2T { runH2T :: m x }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans H2T where
  lift = H2T

instance (MonadH2T m) => MonadTarget (H2T m) where
  tload addr = lift $ do
    tput 0
    tput (2 * fromIntegral addr)
    checkResponse (fromIntegral <$> tget)

  tstore addr value = lift $ do
    tput 1
    tput $ fromIntegral value
    tput (2 * fromIntegral addr)

  tpush value = lift $ do
    tput 2
    tput $ fromIntegral value

  tpop = lift $ do
    tput 3
    checkResponse (fromIntegral <$> tget)

  tcall addr = lift $ do
    tput 4
    tput (2 * fromIntegral addr)
