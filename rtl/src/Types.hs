{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where

import Clash.Prelude hiding (Word, cycle)
import GHC.Generics

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader

type Word = BitVector 16
type Addr = BitVector 16
type SP = BitVector 8

data IS = IS
  { _isMData :: Word
  , _isDData :: Word
  , _isRData :: Word
  } deriving (Show)
makeLenses ''IS

data MS = MS
  { _msDPtr :: SP
  , _msRPtr :: SP
  , _msPC :: Addr
  , _msT :: Word
  , _msLoadFlag :: Bool
  } deriving (Show, Generic, ShowX)
makeLenses ''MS

-- At reset, pretend we're in the second phase of a load. The undefined initial
-- memory contents will overwrite T, and the PC will roll over to 0.
instance Default MS where
  def = MS
    { _msDPtr = 0
    , _msRPtr = 0
    , _msPC = 0xFFFF
    , _msT = 0
    , _msLoadFlag = True
    }

data OS = OS
  { _osMWrite :: Maybe (Addr, Word)
  , _osMRead :: Maybe Addr
  , _osDOp :: (SP, Maybe Word)
  , _osROp :: (SP, Maybe Word)
  } deriving (Show, Generic, ShowX)
makeLenses ''OS
