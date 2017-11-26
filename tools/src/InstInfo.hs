module InstInfo where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Bits

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as LM
import Text.Printf

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
  0 -> t
  1 -> n
  2 -> binaryC "+"
  3 -> binaryC "&"
  4 -> binaryC "|"
  5 -> binaryC "^"
  6 -> "~" <> t
  7 -> binaryC "="
  8 -> binary "<"
  9 -> binary ">>"
  10 -> binary "-"
  11 -> r
  12 -> "[" <> t <> "]"
  13 -> binary "<<"
  14 -> "depth"
  15 -> binary "U<"
  where binary s = n <> s <> t  -- non-commutative
        binaryC s | n <= t = n <> s <> t  -- commutative
                  | otherwise = t <> s <> n

-- | Produces a Forth-style stack effect diagram given before and after stacks.
-- Any common suffix will be hidden.
effect xs ys = picture xs' <> " -- " <> picture ys'
  where
    (xs', ys') = normalize xs ys
    picture ss = intercalate " " $ reverse ss

    normalize [] [] = ([], [])
    normalize xs [] = (xs, [])
    normalize [] ys = ([], ys)
    normalize (x : xs) (y : ys) = case normalize xs ys of
      ([], []) | x == y -> ([], [])
      (xs', ys') -> (x : xs', y : ys')

-- | Abstract-evaluates an instruction given data and return stacks. Produces
-- the new data and return stacks, and any effect on PC and memory,
-- respectively.
eval :: [String] -> [String] -> Int -> ([String], [String], Maybe String, Maybe String)
eval (t : ds) (r : rs) inst =
  let rp = inst .&. (1 `shiftL` 12) /= 0
      tm = (inst `shiftR` 8) .&. 0xF
      tn = inst .&. (1 `shiftL` 7) /= 0
      tr = inst .&. (1 `shiftL` 6) /= 0
      nm = inst .&. (1 `shiftL` 5) /= 0
      radj = (inst `shiftR` 2) .&. 3
      dadj = inst .&. 3

      t' = tmux tm t (head ds) r
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
      aluInsts = [i | i <- [0x6000 .. 0x7FEF], (i .&. 0x10) == 0]
      m = M.fromList $
          map (\i -> (eval ds rs i, i)) aluInsts
      pairs = [(i1, i2) | i1 <- aluInsts, i2 <- aluInsts]
  in LM.fromDistinctAscList $
     mapMaybe (\(i1, i2) -> do
          eff <- evalPair ds rs i1 i2
          iF <- M.lookup eff m
          pure ((i1, i2), (eff, iF)))
        pairs

-- | Prints fusion opportunities to stdout in human-readable format.
showFusionPairs :: IO ()
showFusionPairs = forM_ (M.toList lazyFusionMap) $ \((i1, i2), (eff, iF)) -> do
  printf "Pair %04x %04x -> %04x -  effect %s\n" i1 i2 iF (show eff)
