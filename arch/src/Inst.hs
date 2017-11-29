{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Inst
  ( Inst(..)
  , FlowOrALUInst(..)
  , TMux(..)
  , Reserved(..)
  , canonicalInst
  ) where

import Clash.Prelude hiding (Word)
import Test.QuickCheck

-- | CFM instructions. This type is designed so that its default SOP bit
-- encoding in Clash exactly matches the CFM instruction encoding. (There is
-- also a 'BitPack' instance for moving between this type and 'Word' explicitly.)
data Inst = NotLit FlowOrALUInst
              -- ^ Instructions other than 'Lit' -- MSB 0.
          | Lit (BitVector 15)
              -- ^ Push a zero-extended 15-bit literal -- MSB 1.
          deriving (Eq, Show)

-- | 15-bit encoding of instructions other than 'Lit'.
data FlowOrALUInst = Jump (BitVector 13)
                      -- ^ 00: Jump to a zero-extended 13-bit word address.
                   | JumpZ (BitVector 13)
                      -- ^ 01: Jump if T is zero, popping the data stack.
                   | Call (BitVector 13)
                      -- ^ 10: Call a zero-extended 13-bit word address, pushing
                      -- PC+1 onto the return stack.
                      -- | 11: Generic ALU operation.
                   | ALU Bool -- ^ RP: R to PC.
                         TMux -- ^ Selects the next value of T.
                         Bool -- ^ TN: T to N.
                         Bool -- ^ TR: T to R.
                         Bool -- ^ NM: N to memory at @[T]@.
                         (Reserved Bit)
                         (Signed 2) -- ^ Adjustment to return stack pointer.
                         (Signed 2) -- ^ Adjustment to data stack pointer.
                   deriving (Eq, Show)

-- | Enumerates the possible functions that can be used to compute the next
-- value of T.
data TMux = T         -- ^ Same value as this cycle.
          | N         -- ^ Next cell on data stack.
          | TPlusN    -- ^ Add N.
          | TAndN     -- ^ Bitwise AND with N.
          | TOrN      -- ^ Bitwise OR with N.
          | TXorN     -- ^ Bitwise XOR with N.
          | NotT      -- ^ Complement (bitwise negate) T.
          | NEqT      -- ^ Set all bits if N = T, clear otherwise.
          | NLtT      -- ^ Set all bits if N < T (signed), clear otherwise.
          | NRshiftT  -- ^ N shifted right by T mod 16 positions.
          | NMinusT   -- ^ N - T
          | R         -- ^ Top of return stack.
          | MemAtT    -- ^ Value loaded from memory at @[T]@.
          | NLshiftT  -- ^ N shifted left by T mod 16 positions.
          | Depth     -- ^ Current depth of data stack, zero-extended.
          | NULtT     -- ^ Set all bits if N < T (unsigned), clear otherwise.
          deriving (Eq, Enum, Bounded, Show)

newtype Reserved x = Res x deriving (Eq, Show, BitPack, Arbitrary)

-- | Predicate for filtering out instructions with reserved bits set.
canonicalInst :: Inst -> Bool
canonicalInst (NotLit (ALU _ _ _ _ _ (Res 1) _ _)) = False
canonicalInst _ = True

-------------------------------------------------------------------------------------
-- BitPack instances. These are carefully designed to exactly match Clash's
-- default representation, to avoid overhead when using 'Inst' in synthesizable code.

instance BitPack Inst where
  type BitSize Inst = 16

  pack (NotLit i) = 0 ++# pack i
  pack (Lit v) = 1 ++# v

  unpack (split -> (b15, rest)) = case b15 of
    0 -> NotLit $ unpack rest
    _ -> Lit rest

instance BitPack FlowOrALUInst where
  type BitSize FlowOrALUInst = 15

  pack (Jump v) = 0b00 ++# v
  pack (JumpZ v) = 0b01 ++# v
  pack (Call v) = 0b10 ++# v
  pack (ALU rpc t' tn tr nm xx rd dd) = 0b11 ++#
                                        pack (rpc, t', tn, tr, nm, xx, rd, dd)

  unpack (split -> (top2, rest)) = case top2 of
    0b00 -> Jump rest
    0b01 -> JumpZ rest
    0b10 -> Call rest
    _    -> let (rpc, t', tn, tr, nm, xx, rd, dd) = unpack rest
            in ALU rpc t' tn tr nm xx rd dd

instance BitPack TMux where
  type BitSize TMux = 4
  pack = fromIntegral . fromEnum
  unpack = toEnum . fromIntegral

-------------------------------------------------------------------------------------
-- The always-useful Arbitrary.

instance Arbitrary Inst where
  arbitrary = oneof [ NotLit <$> arbitrary
                    , Lit <$> arbitrary
                    ]

instance Arbitrary FlowOrALUInst where
  arbitrary = oneof [ Jump <$> arbitrary
                    , JumpZ <$> arbitrary
                    , Call <$> arbitrary
                    , ALU <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary TMux where arbitrary = arbitraryBoundedEnum

-------------------------------------------------------------------------------------
-- Useful typeclasses defined in terms of Inst's binary representation.

instance Enum Inst where
  toEnum = unpack . fromIntegral
  fromEnum = fromIntegral . pack

instance Bounded Inst where
  minBound = unpack 0
  maxBound = unpack (-1)

instance Ord Inst where
  a `compare` b = pack a `compare` pack b
