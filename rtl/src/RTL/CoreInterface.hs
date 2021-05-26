{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module RTL.CoreInterface where

import Clash.Prelude hiding (cycle)

import Control.DeepSeq (NFData)
import Control.Lens hiding ((:>))
import Test.QuickCheck

import CFM.Types
import CFM.Inst (Space(..))

type BusReq = Maybe (CellAddr, Maybe Cell)

data BusState = BusFetch
                -- ^ The bus was used to fetch; response is an instruction.
              | BusData Bool
                -- ^ The bus was used for something else. The bool flag
                -- indicates that the bus response should be written to T.
  deriving (Eq, Show, Generic, ShowX, NFData, NFDataX)

instance Arbitrary BusState where
  arbitrary = oneof [ pure BusFetch
                    , BusData <$> arbitrary
                    ]

data IS = IS
  { _isMData :: Cell  -- ^ Response from M port
  , _isIData :: Cell  -- ^ Response from I port
  , _isDData :: Cell  -- ^ Response from data stack
  , _isRData :: Cell  -- ^ Response from return stack
  } deriving (Show, Generic, ShowX, NFData)
makeLenses ''IS

data MS = MS
  { _msDPtr :: SP
  , _msRPtr :: SP
  , _msPC :: CellAddr
  , _msT :: Cell
  , _msBusState :: BusState
  , _msLastSpace :: Space
  } deriving (Show, Generic, ShowX, NFData, NFDataX)
makeLenses ''MS

-- At reset, pretend we're in the second phase of a store. We'll ignore the
-- undefined memory contents and then fetch 0.
instance Default MS where
  def = MS
    { _msDPtr = 0
    , _msRPtr = 0
    , _msPC = 0
    , _msT = 0
    , _msBusState = BusData False
    , _msLastSpace = MSpace
    }

data OS = OS
  { _osMReq :: BusReq
  , _osIReq :: BusReq
  , _osDOp :: (SP, SDelta, Maybe Cell)
  , _osROp :: (SP, SDelta, Maybe Cell)
  , _osFetch :: Bool
  } deriving (Show, Generic, ShowX, NFData)
makeLenses ''OS
