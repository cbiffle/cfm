module Target where

import CFM.Types

-- | Class of monads that describe a CFM target machine.
--
-- These operations are derived from Frank Sergeant's three-instruction Forth
-- model, but with additional stack-specific operations added, since the CFM's
-- stacks are not in addressable memory.
class (Monad m) => MonadTarget m where
  -- | Load a word from memory or I/O. If an error has occured in the target,
  -- it will be returned in place of the result.
  tload :: CellAddr -> m (Either Cell Cell)
  -- | Store a word to memory or I/O.
  tstore :: CellAddr -> Cell -> m ()

  -- | Push a word onto the parameter stack.
  tpush :: Cell -> m ()
  -- | Pop a word from the parameter stack. If an error has occurred in the
  -- target, it will be returned in place of the result.
  tpop :: m (Either Cell Cell)

  -- | Call a subroutine at the given address for its side effects.
  tcall :: CellAddr -> m ()
