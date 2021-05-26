{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Target.Emu where

import Data.Word
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import Data.Maybe (fromMaybe)
import Data.Default
import Data.Bits
import Control.Lens
import Control.Monad.State

import RTL.Beh as R
import RTL.CoreInterface
import Target (MonadTarget(..))

data S = S
  { _sMS :: !MS
  , _sLastOS :: !OS
  , _sMEM :: !(Vector Word16)
  , _sDS :: !(Vector Word16)
  , _sRS :: !(Vector Word16)
  , _sCyc :: !Int
  } deriving (Show)

makeLenses ''S

withMem :: Vector Word16 -> S
withMem mem = S
    { _sMS = def
    , _sLastOS = OS Nothing Nothing (0, 0, Nothing) (0, 0, Nothing) False
    , _sMEM = mem
    , _sDS = V.replicate 256 0xDDDD
    , _sRS = V.replicate 256 0x4444
    , _sCyc = 0
    }

inputs :: S -> IS
inputs s = IS mdata idata ddata rdata
  where
    mdata = fromIntegral $ case s ^. sLastOS . osMReq of
      Just (a, _) ->
        fromMaybe (error "bad M addr") $ s ^? sMEM . ix (fromIntegral a)
      _ -> error "M access on I/O cycle"

    idata = case s ^.sLastOS . osIReq of
      Just (a, _) -> error $ "unimplemented I/O " ++ show a
      _ -> error "I/O access on memory cycle"

    stack opl srl = case s ^. sLastOS . opl of
      (_, _, Just v) -> v
      (p, _, _) -> fromIntegral $
                   fromMaybe (error "SP") $
                   s ^? srl . ix (fromIntegral p)

    ddata = stack osDOp sDS
    rdata = stack osROp sRS

step :: S -> S
step s = S ms' os' mem' ds' rs' cyc'
  where
    is = inputs s
    (ms', os') = R.datapath (s ^. sMS) is

    mem' = case s ^. sLastOS . osMReq of
      Just (a, Just v) -> set (ix (fromIntegral a)) (fromIntegral v) (s ^. sMEM)
      _ -> s ^. sMEM

    ds' = case s ^. sLastOS . osDOp of
      (sp, _, Just v) -> set (ix (fromIntegral sp)) (fromIntegral v) (s ^. sDS)
      _ -> s ^. sDS

    rs' = case s ^. sLastOS . osROp of
      (sp, _, Just v) -> set (ix (fromIntegral sp)) (fromIntegral v) (s ^. sRS)
      _ -> s ^. sRS

    cyc' = s ^. sCyc + 1

newtype EmuT m x = EmuT { runEmuT :: StateT S m x }
  deriving (Functor, Applicative, Monad, MonadState S, MonadIO, MonadFail)

instance (Monad m, MonadFail m) => MonadTarget (EmuT m) where
  tload a = do
    Just v <- preuse $ sMEM . ix (fromIntegral a)
    pure $ Right $ fromIntegral v

  tstore a v = sMEM . ix (fromIntegral a) .= fromIntegral v

  tpush x = do
    let large = x .&. 0x8000 /= 0
    tstore 0x1FFE $ 0x8000 .|. (if large then complement x else x)
    tstore 0x1FFF $ if large then 0x760C else 0x700C

    tcall 0x1FFE

  tpop = do
    v <- use $ sMS . msT
    tstore 0x1FFF 0x710F
    tcall 0x1FFF
    pure $ Right v

  tcall x = do
    -- Prepare CPU to execute from $7FFA
    sMS . msPC .= 0x3FFD
    sMS . msBusState .= BusData False

    -- Assemble there a call to the requested address, and a trap.
    tstore 0x3FFD $ 0x4000 .|. fromIntegral x
    tstore 0x3FFE 0x6000
    tstore 0x3FFF 0x6000

    -- On the first transition the processor will issue a fetch.
    modify step
    -- On the second, it will advance the PC.
    modify step

    -- Now, we just need to wait until the PC comes back.
    let go = do
          modify step
          pc <- use $ sMS . msPC
          if pc == 0x3FFF
            then pure ()
            else go
    go

cycles :: (Monad m) => EmuT m Int
cycles = EmuT $ use sCyc

runEmu :: Monad m => EmuT m x -> m x
runEmu a = evalStateT (runEmuT a) (withMem $ V.replicate 16384 0xDEAD)
