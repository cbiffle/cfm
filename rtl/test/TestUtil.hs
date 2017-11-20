{-# LANGUAGE NoImplicitPrelude #-}
module TestUtil where

import Clash.Prelude
import Test.QuickCheck hiding ((.&.))

import Types

newtype Fetch = Fetch MS deriving (Show)

instance Arbitrary Fetch where
  arbitrary = Fetch <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> pure False)

newtype Load = Load MS deriving (Show)

instance Arbitrary Load where
  arbitrary = Load <$> (MS <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> pure True)
