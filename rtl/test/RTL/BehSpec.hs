{-# LANGUAGE NoImplicitPrelude #-}
module RTL.BehSpec where

import Clash.Prelude
import RTL.Beh
import RTL.TestUtil

spec = genspec datapath
