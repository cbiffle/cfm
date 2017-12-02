module InstInfo where

import Clash.Class.BitPack

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as LM
import Text.Printf

import Inst

-- | Stack effect simulator. Given the SP delta, optional update, and the
-- prior contents of the stack, produces a derived stack.
--
-- Note that this function is poorly behaved if the stack contains fewer than
-- three elements.
stack 0 up (x : xs) = fromMaybe x up : xs

stack 1 up xs = fromMaybe "???" up : xs

stack 3 up (_ : x : xs) = fromMaybe x up : xs

stack 2 up (_ : _ : x : xs) = fromMaybe x up : xs

stack x y z = error $ "stack " ++ show x ++ " " ++ show y ++ " " ++ show z

-- | Symbolic expression generator for the ALU mux.
tmux v t n r = case v of
  T        -> t
  N        -> n
  TPlusN   -> binaryC "+"
  TAndN    -> binaryC "&"
  TOrN     -> binaryC "|"
  TXorN    -> binaryC "^"
  NotT     -> case t of
                '~' : rest -> rest
                _ -> "~" <> t
  NEqT     -> binaryC "="
  NLtT     -> binary "<"
  NRshiftT -> binary ">>"
  NMinusT  -> binary "-"
  R        -> r
  MemAtT   -> "[" <> t <> "]"
  NLshiftT -> binary "<<"
  Depth    -> "depth"
  NULtT    -> binary "U<"
  where binary s = p n <> s <> p t  -- non-commutative
        binaryC s | n <= t = p n <> s <> p t  -- commutative
                  | otherwise = p t <> s <> p n
        p s = "(" <> s <> ")"

-- | Produces a Forth-style stack effect diagram given before and after stacks.
-- Any common suffix will be hidden.
effect xs ys = picture xs' <> " -- " <> picture ys'
  where
    (xs', ys') = normalize xs ys
    picture ss = unwords $ reverse ss

    normalize [] [] = ([], [])
    normalize xs [] = (xs, [])
    normalize [] ys = ([], ys)
    normalize (x : xs) (y : ys) = case normalize xs ys of
      ([], []) | x == y -> ([], [])
      (xs', ys') -> (x : xs', y : ys')

-- | Abstract-evaluates an instruction given data and return stacks. Produces
-- the new data and return stacks, and any effect on PC and memory,
-- respectively.
eval :: [String] -> [String] -> Inst
     -> ([String], [String], Maybe String, Maybe String)
eval (t : ds) (r : rs) inst =
  case inst of
    Lit x ->
      ( show x : t : ds
      , r : rs
      , Nothing
      , Nothing
      )
    NotLit (Jump x) ->
      ( t : ds
      , r : rs
      , Just (show x)
      , Nothing
      )
    NotLit (JumpZ x) ->
      ( ds
      , r : rs
      , Just ("(" ++ t ++ "=0 ? " ++ show x ++ " : PC+1)")
      , Nothing
      )
    NotLit (Call x) ->
      ( t : ds
      , "PC+1" : r : rs
      , Just (show x)
      , Nothing
      )
    NotLit (ALU rp tm tn tr nm _ radj dadj) ->
      let t' = tmux tm t (head ds) r
          ds' = stack dadj (if tn then Just t else Nothing) ds
          rs' = stack radj (if tr then Just t else Nothing) (r : rs)
      in  ( t' : ds'
          , rs'
          , if rp then Just r else Nothing
          , if nm then Just ("[" <> t <> "] <- " <> head ds) else Nothing
          )

-- | Evaluates a sequence of two instructions `i1` then `i2`, using the initial
-- stacks `ds` and `rs`, and returns their compound effect in the same format
-- as 'eval' if such an effect can be described. (If both instructions store to
-- memory, or the first returns, the compound effect can't be described.)
evalPair ds rs i1 i2 =
  let (ds', rs', pc, mem) = eval ds rs i1
      (ds'', rs'', pc', mem') = eval ds' rs' i2
      merge (Just _) (Just _) = Nothing
      merge a Nothing = Just a
      merge Nothing a = Just a
      merge _ _ = Just Nothing
  in do -- in Maybe
    -- Allow either instruction to write memory, but not both, since we can't
    -- generate two memory accesses in the fused version.
    mmem <- merge mem mem'
    -- Only produce a result if the first instruction did *not* return.
    maybe (Just (ds'', rs'', pc', mmem)) (const Nothing) pc

-- | Produces a lazy map of instruction pairs to their fused equivalents.
lazyFusionMap =
  let ds = ["a", "b", "c", "d", "e", "f"]
      rs = ["r", "s", "t", "u", "v", "w"]
      aluInsts = [NotLit $ ALU False T False False False (Res 0) 0 0 ..
                  NotLit $ ALU True NULtT True True True (Res 0) (-1) (-1)]
      canAluInsts = [i | i <- aluInsts, canonicalInst i]
      m = M.fromList $
          map (\i -> (eval ds rs i, i)) canAluInsts
      pairs = [(i1, i2) | i1 <- canAluInsts, i2 <- canAluInsts]
  in LM.fromDistinctAscList $
     mapMaybe (\(i1, i2) -> do
          eff <- evalPair ds rs i1 i2
          iF <- M.lookup eff m
          pure ((i1, i2), (eff, iF)))
        pairs

-- | Prints fusion opportunities to stdout in human-readable format.
showFusionPairs :: IO ()
showFusionPairs = forM_ (M.toList lazyFusionMap) $ \((i1, i2), (eff, iF)) -> do
  printf "Pair %04x %04x -> %04x -  effect %s\n"
         (toInteger (pack i1))
         (toInteger (pack i2))
         (toInteger (pack iF))
         (show eff)
