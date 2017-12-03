{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module RTL.CoreInterface where

import Clash.Prelude hiding (Word, cycle)
import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Lens hiding ((:>))
import Test.QuickCheck

import CFM.Types

data Space = MSpace | ISpace
             deriving (Eq, Show, Enum, Bounded, Generic, ShowX, NFData)

instance BitPack Space where
  type BitSize Space = 1
  pack = fromIntegral . fromEnum
  unpack = toEnum . fromIntegral

instance Arbitrary Space where
  arbitrary = arbitraryBoundedEnum

data BusReq = MReq SAddr (Maybe (Space, SAddr, Word))
                -- ^ An M-type request specifies a memory read and a write to
                -- either memory or I/O space.
            | IReq SAddr
                -- ^ An I-type request specifies only an I/O read.
            deriving (Eq, Show, Generic, ShowX, NFData)

instance Arbitrary BusReq where
  arbitrary = oneof [ MReq <$> arbitrary <*> arbitrary
                    , IReq <$> arbitrary
                    ]

data IS = IS
  { _isMData :: Word  -- ^ Response from M port
  , _isIData :: Word  -- ^ Response from I port
  , _isDData :: Word  -- ^ Response from data stack
  , _isRData :: Word  -- ^ Response from return stack
  } deriving (Show, Generic, ShowX, NFData)
makeLenses ''IS

data MS = MS
  { _msDPtr :: SP
  , _msRPtr :: SP
  , _msPC :: SAddr
  , _msT :: Word
  , _msLoadFlag :: Bool
  , _msLastSpace :: Space
    -- Note: if loads don't corrupt T, this value is present in the MSB.
    -- Currently loads defensively corrupt T.
  } deriving (Show, Generic, ShowX, NFData)
makeLenses ''MS

-- At reset, pretend we're in the second phase of a load. The undefined initial
-- memory contents will overwrite T and then we'll fetch 0.
instance Default MS where
  def = MS
    { _msDPtr = 0
    , _msRPtr = 0
    , _msPC = 0
    , _msT = 0
    , _msLoadFlag = True
    , _msLastSpace = MSpace
    }

data OS = OS
  { _osBusReq :: BusReq
  , _osDOp :: (SP, SDelta, Maybe Word)
  , _osROp :: (SP, SDelta, Maybe Word)
  , _osFetch :: Bool
  } deriving (Show, Generic, ShowX, NFData)
makeLenses ''OS
