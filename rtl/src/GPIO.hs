{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module GPIO where

import Clash.Prelude hiding (Word)
import Data.Maybe (fromMaybe)
import Control.Monad (join)

import Types

outport :: (HasClockReset d g s)
        => Signal d (Maybe (BitVector n, Maybe Word))
        -> ( Signal d Word
           , Signal d Word
           )
outport cmd = (resp, outs)
  where
    outs = register 0 $ fromMaybe <$> outs <*> (join . fmap snd <$> cmd)
    resp = outs

inport :: (HasClockReset d g s)
       => Signal d Word
       -> Signal d (Maybe (BitVector n, Maybe Word))
       -> Signal d Word
inport port _ = resp
  where
    resp = register 0 port

