{-# LANGUAGE NoImplicitPrelude #-}
module CFMTop where

import Clash.Prelude
import Data.Tuple (swap)
import CFM

topEntity c r = withClockReset @System @Source @Asynchronous c r $ mealy (\s i -> swap $ cycle' s i) def
