module Target where

import Prelude hiding (Word)

import CFM.Types

-- | Class of monads that describe a CFM target machine.
--
-- These operations are derived from Frank Sergeant's three-instruction Forth
-- model, but with additional stack-specific operations added, since the CFM's
-- stacks are not in addressable memory.
class (Monad m) => MonadTarget m where
  -- | Load a word from memory or I/O.
  tload :: WordAddr -> m Word
  -- | Store a word to memory or I/O.
  tstore :: WordAddr -> Word -> m ()

  -- | Push a word onto the parameter stack.
  tpush :: Word -> m ()
  -- | Pop a word from the parameter stack
  tpop :: m Word

  -- | Push a word onto the return stack.
  tpushR :: Word -> m ()
  -- | Pop a word from the return stack.
  tpopR :: m Word

  -- | Call a subroutine at the given address for its side effects.
  tcall :: WordAddr -> m ()
