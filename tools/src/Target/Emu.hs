{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Target.Emu where

import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Clash.Sized.Vector as CV
import Data.Vector.Unboxed (Vector)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Default
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.Chan

import RTL.Beh as R
import RTL.CoreInterface
import Target (MonadTarget(..))
import Target.Stub (debugStub)
import Target.H2T

data S = S
  { _sMS :: !MS
  , _sLastOS :: !OS
  , _sMEM :: !(Vector Word16)
  , _sDS :: !(Vector Word16)
  , _sRS :: !(Vector Word16)
  , _sH2T :: !(Maybe Word16)
  , _sT2H :: !(Maybe Word16)
  , _sCyc :: !Int
  } deriving (Show)

makeLenses ''S

withMem :: Vector Word16 -> S
withMem mem = S
    { _sMS = def
    , _sLastOS = OS (MReq 0 Nothing) (0, 0, Nothing) (0, 0, Nothing) False
    , _sMEM = mem
    , _sDS = V.replicate 256 0xDDDD
    , _sRS = V.replicate 256 0x4444
    , _sH2T = Nothing
    , _sT2H = Nothing
    , _sCyc = 0
    }

inputs :: S -> IS
inputs s = IS mdata idata ddata rdata
  where
    mdata = fromIntegral $ case s ^. sLastOS . osBusReq of
      MReq a _ -> fromMaybe (error "bad M addr") $ s ^? sMEM . ix (fromIntegral a)
      IReq _ -> error "M access on I/O cycle"

    idata = fromIntegral $ case s ^.sLastOS . osBusReq of
      MReq _ _ -> error "I/O access on M cycle"
      IReq 0 -> maybe 0 (const 0xFFFF) $ s ^. sH2T
      IReq 1 -> fromMaybe (error "H2T empty") $ s ^. sH2T
      IReq 2 -> maybe 0xFFFF (const 0) $ s ^. sT2H
      IReq 3 -> error "T2H write only"
      IReq a -> error $ "unimplemented I/O " ++ show a

    stack opl srl = case s ^. sLastOS . opl of
      (_, _, Just v) -> v
      (p, _, _) -> fromIntegral $
                   fromMaybe (error "SP") $
                   s ^? srl . ix (fromIntegral p)

    ddata = stack osDOp sDS
    rdata = stack osROp sRS

update :: (Maybe Word16, Bool) -> S -> S
update (h, htake) s = S ms' os' mem' ds' rs' h2t' t2h' cyc'
  where
    is = inputs s
    (ms', os') = R.datapath (s ^. sMS) is

    mem' = case s ^. sLastOS . osBusReq of
      MReq _ (Just (MSpace, a, v)) -> set (ix (fromIntegral a)) (fromIntegral v) (s ^. sMEM)
      _ -> s ^. sMEM

    ds' = case s ^. sLastOS . osDOp of
      (sp, _, Just v) -> set (ix (fromIntegral sp)) (fromIntegral v) (s ^. sDS)
      _ -> s ^. sDS

    rs' = case s ^. sLastOS . osROp of
      (sp, _, Just v) -> set (ix (fromIntegral sp)) (fromIntegral v) (s ^. sRS)
      _ -> s ^. sRS

    h2t' | isJust h = h
         | IReq 1 <- s ^. sLastOS . osBusReq = Nothing
         | otherwise = s ^. sH2T

    t2h' | htake = Nothing
         | MReq _ (Just (ISpace, 3, v)) <- s ^. sLastOS . osBusReq = Just (fromIntegral v)
         | otherwise = s ^. sT2H

    cyc' = s ^. sCyc + 1

run :: Vector Word16 -> [(Maybe Word16, Bool)] -> [S]
run mem is = states
  where
    states = zipWith update is $ withMem mem : states

emuStub :: Vector Word16
emuStub = V.fromList $ map fromIntegral $ CV.toList debugStub


newtype IOEmu' x = IOEmu' (ReaderT (Chan (Maybe Word16, Bool))
                          (StateT [(Maybe Word16, Bool, Int)] IO) x)
  deriving (Functor, Applicative, Monad, MonadReader (Chan (Maybe Word16, Bool)),
            MonadState [(Maybe Word16, Bool, Int)], MonadIO)

runIOEmu' :: Vector Word16 -> IOEmu' x -> IO x
runIOEmu' ram (IOEmu' a) = do
  ch <- newChan
  chLazy <- getChanContents ch
  let states = run ram chLazy
      os = map (\s -> (s ^. sT2H, isNothing $ s ^. sH2T, s ^. sCyc)) states
  evalStateT (runReaderT a ch) os

instance MonadH2T IOEmu' where
  tick w b = do
    c <- ask
    liftIO $ writeChan c (fromIntegral <$> w, b)
    (x, y, _) : rest <- get
    put rest
    pure (fromIntegral <$> x, y)

  cycles = do
    c <- ask
    liftIO $ writeChan c (Nothing, False)
    (_, _, x) : rest <- get
    put rest
    pure x

newtype IOEmu x = IOEmu (H2T IOEmu' x)
  deriving (Functor, Applicative, Monad, MonadTarget, MonadIO)

instance MonadH2T IOEmu where
  tick w b = IOEmu $ H2T $ tick w b
  cycles = IOEmu $ H2T cycles

runIOEmu :: Vector Word16 -> IOEmu x -> IO x
runIOEmu mem (IOEmu x) = runIOEmu' mem (runH2T x)
