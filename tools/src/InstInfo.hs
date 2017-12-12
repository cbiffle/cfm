module InstInfo where

import Clash.Class.BitPack
import Clash.Class.Resize (zeroExtend)

import Data.Maybe (fromMaybe)

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import Text.Printf

import CFM.Inst
import CFM.Types

data Expr = V !Int
          | L !Cell
          | Bin !Expr !BOp !Expr
          | Un !UOp !Expr
          | CondZ !Expr !Expr !Expr
          | PCPlus !Int
          | Dep
          | Undef
          deriving (Eq, Show, Ord)

data UOp = NotOp | DerefOp
         deriving (Eq, Show, Ord)

data BOp = AddOp
         | AndOp
         | OrOp
         | XorOp
         | EqOp
         | SignedLessOp
         | RShiftOp
         | MinusOp
         | LShiftOp
         | UnsignedLessOp
         deriving (Eq, Show, Ord)

type Stack = [Expr]

type Effect = (Stack, Stack, Maybe Expr, Maybe (Expr, Expr))

simpl1 :: Expr -> Expr
simpl1 (Un NotOp (Un NotOp x)) = x
simpl1 x = x

-- | Stack effect simulator. Given the SP delta, optional update, and the
-- prior contents of the stack, produces a derived stack.
--
-- Note that this function is poorly behaved if the stack contains fewer than
-- three elements.
stack :: SDelta -> Maybe Expr -> Stack -> Stack
stack 0 up (x : xs) = fromMaybe x up : xs

stack 1 up xs = fromMaybe Undef up : xs

stack 3 up (_ : x : xs) = fromMaybe x up : xs

stack 2 up (_ : _ : x : xs) = fromMaybe x up : xs

stack x y z = error $ "stack " ++ show x ++ " " ++ show y ++ " " ++ show z

-- | Symbolic expression generator for the ALU mux.
tmux :: TMux -> Expr -> Expr -> Expr -> Expr
tmux v t n r = simpl1 $ case v of
  T        -> t
  N        -> n
  TPlusN   -> binaryC AddOp
  TAndN    -> binaryC AndOp
  TOrN     -> binaryC OrOp
  TXorN    -> binaryC XorOp
  NotT     -> Un NotOp t
  NEqT     -> binaryC EqOp
  NLtT     -> binary SignedLessOp
  NRshiftT -> binary RShiftOp
  NMinusT  -> binary MinusOp
  R        -> r
  MemAtT   -> Un DerefOp t
  NLshiftT -> binary LShiftOp
  Depth    -> Dep
  NULtT    -> binary UnsignedLessOp
  where binary s = Bin n s t -- non-commutative
        binaryC s | n <= t = Bin n s t  -- commutative
                  | otherwise = Bin t s n

-- | Abstract-evaluates an instruction given data and return stacks. Produces
-- the new data and return stacks, and any effect on PC and memory,
-- respectively.
eval :: Stack -> Stack -> Int -> Inst -> Effect
eval (t : ds) (r : rs) pc inst =
  case inst of
    Lit x ->
      ( L (zeroExtend x) : t : ds
      , r : rs
      , Nothing
      , Nothing
      )
    NotLit (Jump x) ->
      ( t : ds
      , r : rs
      , Just $ L $ zeroExtend x
      , Nothing
      )
    NotLit (JumpZ x) ->
      ( ds
      , r : rs
      , Just $ simpl1 $ CondZ t (L (zeroExtend x)) (PCPlus (pc+1))
      , Nothing
      )
    NotLit (Call x) ->
      ( t : ds
      , PCPlus (pc+1) : r : rs
      , Just $ L $ zeroExtend x
      , Nothing
      )
    NotLit (ALU rp tm tn tr nm _ radj dadj) ->
      let t' = tmux tm t (head ds) r
          ds' = stack dadj (if tn then Just t else Nothing) ds
          rs' = stack radj (if tr then Just t else Nothing) (r : rs)
      in  ( t' : ds'
          , rs'
          , if rp then Just r else Nothing
          , if nm then Just (t, head ds) else Nothing
          )
eval _ _ _ _ = error "stacks empty in eval"

-- | Evaluates a sequence of two instructions `i1` then `i2`, using the initial
-- stacks `ds` and `rs`, and returns their compound effect in the same format
-- as 'eval' if such an effect can be described. (If both instructions store to
-- memory, or the first returns, the compound effect can't be described.)
evalPair :: Stack -> Stack -> Inst -> Inst -> Maybe Effect
evalPair ds rs i1 i2 =
  let (ds', rs', pc, mem) = eval ds rs 0 i1
      (ds'', rs'', pc', mem') = eval ds' rs' 1 i2
      merge (Just _) (Just _) = Nothing
      merge a Nothing = Just a
      merge Nothing a = Just a
  in do -- in Maybe
    -- Allow either instruction to write memory, but not both, since we can't
    -- generate two memory accesses in the fused version.
    mmem <- merge mem mem'
    -- Only produce a result if the first instruction did *not* return.
    maybe (Just (ds'', rs'', pc', mmem)) (const Nothing) pc

cds, crs :: Stack
cds = map V [0 .. 5]
crs = map V [6 .. 11]

ceval :: Int -> Inst -> Effect
ceval = eval cds crs

cevalPair :: Inst -> Inst -> Maybe Effect
cevalPair = evalPair cds crs

canAluInsts :: [Inst]
canAluInsts =
  let aluInsts = [NotLit $ ALU False T False False False (Res 0) 0 0 ..
                  NotLit $ ALU True NULtT True True True (Res 0) (-1) (-1)]
  in [i | i <- aluInsts, canonicalInst i]

instructionsByEffect :: M.Map Effect Inst
instructionsByEffect = M.fromList $ map (\i -> (ceval 0 i, i)) canAluInsts

fuse :: Inst -> Inst -> Maybe Inst
fuse i1 i2 = do
  eff <- cevalPair i1 i2
  M.lookup eff instructionsByEffect

-- | Prints fusion opportunities to stdout in human-readable format.
showFusionPairs :: IO ()
showFusionPairs =
  forM_ [(i1, i2) | i1 <- canAluInsts, i2 <- canAluInsts] $ \(i1, i2) ->
    case fuse i1 i2 of
      Nothing -> pure ()
      Just iF -> printf "Pair %04x %04x -> %04x -  effect %s\n"
         (toInteger (pack i1))
         (toInteger (pack i2))
         (toInteger (pack iF))
         (show (cevalPair i1 i2))
