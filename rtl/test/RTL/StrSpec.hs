{-# LANGUAGE NoImplicitPrelude #-}
module RTL.StrSpec where

import Clash.Prelude
import RTL.Str
import RTL.TestUtil

spec = genspec datapath
