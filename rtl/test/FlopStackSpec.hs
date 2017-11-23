{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module FlopStackSpec where

import Clash.Prelude hiding (v)
import qualified Data.List as L
import Types
import FlopStack
import Test.Hspec
import Test.QuickCheck

dep = SNat @4

spec = do
  it "reads after write" $ property $ \delta value ->
    let [_, v] = sampleN 2 $ flopStack dep (pure delta) (pure (Just value))
    in v == value

  it "handles delta -1 correctly" $ property $ \xdelta xwrite ->
    let inits = 0 :> 1 :> 2 :> 3 :> Nil
        deltas = [  -1   ,   -1   ,   -1   , xdelta ]
        writes = [Nothing, Nothing, Nothing, xwrite ]
        outs   = [   0   ,    1   ,    2   ,    3   ]
    in (sampleN (L.length deltas) $ flopStack' inits (fromList deltas) (fromList writes)) `shouldBe` outs

{-
  it "handles delta -2 correctly" $ property $ \xdelta xwrite ->
    let inits = 0 :> 1 :> 2 :> 3 :> Nil
        deltas = [  -2   ,   -1   , xdelta ]
        writes = [Nothing, Nothing, xwrite ]
        outs   = [   0   ,    2   ,    3   ]
    in (sampleN (L.length deltas) $ flopStack' inits (fromList deltas) (fromList writes)) `shouldBe` outs
-}

  it "pops after push" $ property $ \v1 v2 xdelta xwrite ->
    let inits = 0 :> 1 :> 2 :> 3 :> Nil
        deltas = [   0   ,    1    ,   -1    , xdelta  ]
        writes = [Just v1, Just v2 , Nothing , xwrite  ]
        outs   = [   0   ,   v1    ,   v2    ,   v1    ]
    in outs == (sampleN (L.length deltas) $ flopStack' inits (fromList deltas) (fromList writes))
