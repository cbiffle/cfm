{-# LANGUAGE NoImplicitPrelude #-}
module BehSpec where

import Clash.Prelude
import Beh
import TestUtil

spec = genspec cycle'
