{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Types where

import Clash.Prelude hiding (Word, cycle)

-- | The width of a word in bits, as a type-level natural.
type Width = 16
-- | A word.
type Word = BitVector Width
-- | The address of a word. Words are byte-addressed, so the bottom bit is omitted.
type WordAddr = BitVector (Width - 1)
-- | A stack pointer.
type SP = BitVector 8
-- | A stack delta, as it appears in the ALU instructions.
type SDelta = Signed 2
