{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
-- | A target that runs the actual processor RTL in an isolated context.
module Target.RTL where

import Prelude hiding (Word)
import qualified Clash.Signal as C
import Clash.Sized.Vector (Vec(..))
import Clash.Sized.BitVector ((++#))
import qualified Clash.Sized.Vector as V
import GHC.TypeLits (KnownNat)
import Data.List (foldl')
import Data.Either (either)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent.Chan

import CFM.Types
import Target
import Assembler (asmQQ)
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
            foldl' (\v (a, i) -> V.replace a i v)
                   (V.repeat 0xDEAD) $
            zip [0 :: Int ..] $
            [asmQQ|
            ( Primitive ALU instruction definitions )
            0x6180 alu: swap          ( a b -- b a )
            0x6020 alu: 2dup/!        ( a b -- a b )
            0x6381 alu: 2dup/and      ( a b -- a b a&b )
            0x6103 alu: drop          ( x -- )
            0x6600 alu: invert        ( x -- ~x )
            0x6203 alu: +             ( a b -- a+b)
            0x6a03 alu: -             ( a b -- a-b)
            0x6903 alu: rshift        ( a b -- a>>b )
            0x6d03 alu: lshift        ( a b -- a<<b )
            0x6303 alu: and           ( a b -- a&b)
            0x6403 alu: or            ( a b -- a|b)
            0x6081 alu: dup           ( x -- x x )
            0x6c00 alu: @             ( x -- [x] )
            0x6703 alu: =             ( a b -- a=b )
            0x6f03 alu: u<            ( a b -- a<b )
            0x6803 alu: <             ( a b -- a<b )
            0x6181 alu: over          ( a b -- a b a )
            0x6e81 alu: depth         ( a b -- a b a )
            0x6147 alu: >r            ( a --  R: -- a )
            0x6b8d alu: r>            ( -- a  R: a -- )

            0x3E00 org  ( leave most of RAM available )

            0x8000 constant in-ready?   ( reads non-zero when a word awaits )
            0x8002 constant in-value    ( reads as last word from host, clears in-ready)
            0x8004 constant out-ready?  ( reads non-zero when no outgoing word waits )
            0x8006 constant out-value   ( writes go to host )

            : execute  ( i*x xt -- j*x )  >r ;

            : >host  ( x -- )
              begin out-ready? @ until  ( spin until FIFO available )
              out-value ! ;             ( send word )

            : host>  ( -- x )
              begin in-ready? @ until   ( spin until word available )
              in-value @ ;              ( receive word )

            : debug-loop
              host>
              0 over = if drop  ( peek )
                host>
                0 >host   ( command valid )
                @ >host   ( result of load )
                debug-loop exit
              then
              1 over = if drop  ( poke )
                host> host>
                0 >host   ( command valid )
                !
                debug-loop exit
              then
              2 over = if drop  ( push )
                host>
                0 >host
                debug-loop exit
              then
              3 over = if drop  ( pop )
                0 >host
                >host
                debug-loop exit
              then
              4 over = if drop  ( >r )
                host>
                0 >host
                >r
                debug-loop exit
              then
              5 over = if drop  ( r> )
                0 >host
                r> >host
                debug-loop exit
              then
              6 over = if drop  ( execute )
                host>
                0 >host
                execute
                0 >host
                debug-loop exit
              then

              drop
              1 >host ( command not recognized)
              debug-loop ;

            : debug
              0 0 !   ( clear the reset vector )
              debug-loop ;

            0 org
            : reset debug ;|]
