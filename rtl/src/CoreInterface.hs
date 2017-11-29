{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module CoreInterface where

import Clash.Prelude hiding (Word, cycle)
import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Lens hiding ((:>))

import Types

data IS = IS
  { _isMData :: Word
  , _isDData :: Word
  , _isRData :: Word
  } deriving (Show, Generic, ShowX, NFData)
makeLenses ''IS

data MS = MS
  { _msDPtr :: SP
  , _msRPtr :: SP
  , _msPC :: WordAddr
  , _msT :: Word
  , _msLoadFlag :: Bool
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
    }

data OS = OS
  { _osMWrite :: Maybe (WordAddr, Word)
  , _osMRead :: WordAddr
  , _osDOp :: (SP, SDelta, Maybe Word)
  , _osROp :: (SP, SDelta, Maybe Word)
  } deriving (Show, Generic, ShowX, NFData)
makeLenses ''OS
