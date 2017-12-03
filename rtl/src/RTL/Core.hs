{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
module RTL.Core where

import Clash.Prelude hiding (Word, readIO, read)
import Control.Lens hiding ((:>), (:<), op)
import CFM.Types
import RTL.Str
import RTL.CoreInterface

-- | Registered version of the core datapath.
core :: HasClockReset dom gated synchronous
     => Signal dom IS -> Signal dom OS
core = mealy datapath def

-- | Combines 'core' with the selected implementation of stacks, and exposes
-- the local bus interface.
coreWithStacks
  :: (HasClockReset dom gated synchronous)
  => Signal dom Word    -- ^ read response from memory
  -> Signal dom Word    -- ^ read response from I/O
  -> ( Signal dom BusReq
     , Signal dom Bool
     )  -- ^ Bus request and fetch signal, respectively.
coreWithStacks mresp ioresp = (busReq, fetch)
  where
    coreOuts = core $ IS <$> mresp <*> ioresp <*> n <*> r

    busReq = coreOuts <&> (^. osBusReq)
    fetch = coreOuts <&> (^. osFetch)

    n = stack "D" $ coreOuts <&> (^. osDOp)
    r = stack "R" $ coreOuts <&> (^. osROp)

stack :: (HasClockReset d g s)
      => String -> Signal d (SP, SDelta, Maybe Word) -> Signal d Word
stack name op = readNew (blockRamPow2 (repeat $ errorX name))
                        (op <&> (^. _1) <&> unpack)
                        (op <&> repackStack)
  where
    repackStack (_, _, Nothing) = Nothing
    repackStack (a, _, Just v) = Just (unpack a, v)

-- | Combines 'coreWithStacks' with a RAM built from the given constructor, and
-- an I/O bridge, exposing the I/O bus.
coreWithRAM
  :: (HasClockReset dom gated synchronous)
  => (Signal dom SAddr -> Signal dom (Maybe (SAddr, Word)) -> Signal dom Word)
    -- ^ RAM constructor
  -> Signal dom Word    -- ^ I/O read response, valid when addressed.
  -> ( Signal dom (Maybe (SAddr, Maybe Word))
     , Signal dom Bool
     ) -- ^ I/O bus outputs and fetch signal, respectively.
coreWithRAM ram ioresp = (ioreq, fetch)
  where
    (busReq, fetch) = coreWithStacks mresp ioresp

    -- Memory reads on a blockRam do not have an enable line, i.e. a read
    -- occurs every cycle whether we like it or not. Since the reads are not
    -- effectful, that's okay, and we route the address bits to RAM independent
    -- of the type of request to save hardware.
    mread = busReq <&> \b -> case b of
      MReq a _ -> a
      IReq a   -> a

    -- Memory writes can only occur from an MReq against MSpace.
    mwrite = busReq <&> \b -> case b of
      MReq _ (Just (MSpace, a, v)) -> Just (a, v)
      _                            -> Nothing

    -- IO requests are either IReqs (reads) or MReqs against ISpace (writes).
    ioreq = busReq <&> \b -> case b of
      IReq a                       -> Just (a, Nothing)
      MReq _ (Just (ISpace, a, v)) -> Just (a, Just v)
      _                            -> Nothing

    mresp = ram mread mwrite
