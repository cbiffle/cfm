{-# LANGUAGE NoImplicitPrelude #-}

module RTL.Common.Strobe where

import Clash.Prelude
import Data.Maybe (isJust)

class Strobe t where
  strobeValue :: t

toStrobe :: (Strobe t) => Bool -> Maybe t
toStrobe True = Just strobeValue
toStrobe False = Nothing

fromStrobe :: (Strobe t) => Maybe t -> Bool
fromStrobe = isJust
