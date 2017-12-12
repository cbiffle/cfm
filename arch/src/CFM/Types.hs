{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module CFM.Types where

import Clash.Prelude hiding (cycle)

-- | The width of a word in bits, as a type-level natural.
type Width = 16
-- | A memory cell.
type Cell = BitVector Width
-- | The address of a cell. Cellss are byte-addressed, so the bottom bit is
-- omitted.
type CellAddr = BitVector (Width - 1)
-- | The address of a word in either the memory or I/O space.
type SAddr = BitVector (Width - 2)
-- | A stack pointer.
type SP = BitVector 8
-- | A stack delta, as it appears in the ALU instructions.
type SDelta = Signed 2
