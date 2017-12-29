{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- | IO bus to SRAM interface
module RTL.SRAM where

import Clash.Prelude
import Data.Maybe (fromMaybe, isJust)

import CFM.Types

extsram :: (HasClockReset d g s)
        => Signal d Cell  -- ^ SRAM-to-host data bus
        -> Signal d (Maybe (BitVector a, Maybe Cell)) -- ^ IOBus
        -> ( Signal d Cell
           , Signal d (BitVector a)
           , Signal d Bool
           , Signal d Cell
           )  -- ^ IO response, SRAM address, SRAM write, host-to-SRAM data
extsram = curry $ mealyB extsramT def

data S a = S
  { sA :: BitVector a
  , sWD :: Maybe Cell
  } deriving (Show)

instance Default (S a) where def = S def def

extsramT :: S a
         -> (Cell, Maybe (BitVector a, Maybe Cell))
         -> (S a, (Cell, BitVector a, Bool, Cell))
extsramT s (s2h, ioreq) = (s', (s2h, sA s, isJust (sWD s), h2s))
  where
    s' = uncurry S $ fromMaybe (sA s, Nothing) ioreq
    h2s = fromMaybe (errorX "SRAM D undefined during read") (sWD s)
