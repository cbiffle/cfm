{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}
module RTL.Core where

import Clash.Prelude hiding (readIO, read)
import Control.Lens hiding ((:>), (:<), op)
import CFM.Types
import RTL.Str
import RTL.CoreInterface

-- | Registered version of the core datapath.
core :: HiddenClockResetEnable dom
     => Signal dom IS -> Signal dom OS
core = mealy datapath def

-- | Combines 'core' with the selected implementation of stacks, and exposes
-- the local bus interface.
coreWithStacks
  :: (HiddenClockResetEnable dom)
  => Signal dom Cell    -- ^ read response from memory
  -> Signal dom Cell    -- ^ read response from I/O
  -> ( Signal dom BusReq
     , Signal dom BusReq
     , Signal dom Bool
     )  -- ^ Memory and I/O Bus requests and fetch signal, respectively.
coreWithStacks mresp ioresp = (mreq, ireq, fetch)
  where
    coreOuts = core $ IS <$> mresp <*> ioresp <*> n <*> r

    mreq = coreOuts <&> (^. osMReq)
    ireq = coreOuts <&> (^. osIReq)
    fetch = coreOuts <&> (^. osFetch)

    n = stack "D" $ coreOuts <&> (^. osDOp)
    r = stack "R" $ coreOuts <&> (^. osROp)

stack :: (HiddenClockResetEnable d)
      => String -> Signal d (SP, SDelta, Maybe Cell) -> Signal d Cell
stack name op = readNew (blockRamPow2 (repeat $ errorX name))
                        (op <&> (^. _1) <&> unpack)
                        (op <&> repackStack)
  where
    repackStack (_, _, Nothing) = Nothing
    repackStack (a, _, Just v) = Just (unpack a, v)

-- | Combines 'coreWithStacks' with a RAM built from the given constructor, and
-- an I/O bridge, exposing the I/O bus.
coreWithRAM
  :: (HiddenClockResetEnable dom)
  => (Signal dom (Maybe (CellAddr, Maybe Cell)) -> Signal dom Cell)
    -- ^ RAM constructor
  -> Signal dom Cell    -- ^ I/O read response, valid when addressed.
  -> ( Signal dom (Maybe (CellAddr, Maybe Cell))
     , Signal dom Bool
     ) -- ^ I/O bus outputs and fetch signal, respectively.
coreWithRAM ram ioresp = (ioreq, fetch)
  where
    (mreq, ioreq, fetch) = coreWithStacks mresp ioresp
    mresp = ram mreq

singlePorted
  :: (Signal dom CellAddr
       -> Signal dom (Maybe (CellAddr, Cell))
       -> Signal dom Cell)
  -> Signal dom (Maybe (CellAddr, Maybe Cell))
  -> Signal dom Cell
singlePorted ram mreq = ram rd wr
  where
    rd = maybe undefined fst <$> mreq
    wr = mreq <&> \r -> case r of
                          Just (a, Just v) -> Just (a, v)
                          _ -> Nothing
