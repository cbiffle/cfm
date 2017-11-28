{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Text.Printf

import Data.Bits
import Data.Default
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO (openFile, IOMode(WriteMode), hClose)

import Control.Monad.State
import Control.Monad.Except

import Parser
import qualified InstInfo as II

finally :: (MonadError e m) => m a -> m b -> m a
finally body handler = (body <* handler) `catchError`
                          \e -> handler >> throwError e

data Def = Compiled Int         -- ^ Callable address.
         | InlineLit Int        -- ^ 15-bit literal.
         | RawInst Int          -- ^ Arbitrary 16-bit instruction
         | Immediate (Asm ())   -- ^ Action to be performed

instance Show Def where
  show (Compiled x) = "Compiled " ++ show x
  show (InlineLit x) = "InlineLit " ++ show x
  show (RawInst x) = "RawInst " ++ show x
  show (Immediate _) = "Immediate ..."

data Val = Data Int
         | Inst Int
         deriving (Show, Eq)
deval :: Val -> Int
deval (Data x) = x
deval (Inst x) = x

data AS = AS
  { asDict :: M.Map String Def
  , asStack :: [Int]
  , asPos :: Int
  , asMem :: M.Map Int Val
  , asCanFuse :: Bool
  , asCompiling :: Bool
  } deriving (Show)

instance Default AS where
  def = AS M.empty [] 0 M.empty False False

newtype Asm a = Asm { runAsm :: ExceptT String (State AS) a }
  deriving (Functor, Applicative, Monad, MonadState AS, MonadError String)

blockFusion, allowFusion :: Asm ()
blockFusion = modify $ \s -> s { asCanFuse = False }
allowFusion = modify $ \s -> s { asCanFuse = True }

push :: Int -> Asm ()
push x = modify $ \s -> s { asStack = x : asStack s }

pop :: Asm Int
pop = do
  ss <- gets asStack
  case ss of
    [] -> throwError "stack underflow in interpreted code"
    (x : xs) -> do
      modify $ \s -> s { asStack = xs }
      pure x

here :: Asm Int
here = gets asPos

create :: String -> Def -> Asm ()
create n d = modify $ \s -> s { asDict = M.insert n d (asDict s) }

find :: String -> Asm Def
find n = do
  md <- gets (M.lookup n . asDict)
  case md of
    Nothing -> case reads n of
      [(x, "")] -> pure (InlineLit x)
      _         -> throwError ("use of unknown word " ++ n)
    Just d -> pure d

store :: Val -> Int -> Asm ()
store v a = do
  prev <- gets $ M.lookup a . asMem
  case prev of
    Nothing ->  modify $ \s -> s { asMem = M.insert a v (asMem s) }
    Just v' | v == v' -> modify $ \s -> s { asMem = M.insert a v (asMem s) }
    Just v' -> throwError ("Address " ++ show a ++ " previously set to " ++
                           show v' ++ " can't be changed to " ++ show v)

patch :: Val -> Val -> Int -> Asm ()
patch p v a = do
  prev <- gets $ M.lookup a . asMem
  case prev of
    Nothing ->  modify $ \s -> s { asMem = M.insert a v (asMem s) }
    Just v' | p == v' -> modify $ \s -> s { asMem = M.insert a v (asMem s) }
    Just v' -> throwError ("Address " ++ show a ++ " previously set to " ++
                           show v' ++ " can't be changed to " ++ show v)

commaVal :: Val -> Asm ()
commaVal v = do
  here >>= store v
  modify $ \s -> s { asPos = asPos s + 1 }

comma :: Int -> Asm ()
comma v | 0 <= v && v < 65536 = commaVal (Data v)
comma _ = error "internal error: value passed to comma out of range"

cComma :: Int -> Asm ()
cComma v | 0 <= v && v < 65536 = do
  canFuse <- gets asCanFuse
  allowFusion
  if not canFuse
    then commaVal (Inst v)
    else do
      -- Inspect the most recently compiled instruction.
      end <- here
      lastInst <- gets $ M.lookup (end - 1) . asMem
      case lastInst of
        Just (Inst i) -> case M.lookup (i, v) II.lazyFusionMap of
          Just (_, iF) -> patch (Inst i) (Inst iF) (end - 1)
          Nothing -> case (i .&. 0xE000, v) of
            -- Call-Return: convert to jump (tail-call optimization)
            (0x4000, 0x700C) -> patch (Inst i) (Inst (i .&. 0x1FFF)) (end - 1)
            -- Jump-Return: elide return, e.g. "begin again ;"
            (0x0000, 0x700C) -> pure ()
            -- Otherwise, compile it.
            _ -> commaVal (Inst v)
        -- No previous instruction? Must be after an org directive. Don't fuse.
        _ -> commaVal (Inst v)
cComma _ = error "internal error: value passed to cComma out of range"

exit :: Asm ()
exit = do
  cComma 0x700C
  blockFusion

run :: AsmTop -> Asm ()
 
run (Colon name body) = do
  create name . Compiled =<< here
  modify $ \s -> s { asCompiling = True }
  (mapM_ compile' body >> exit) `finally`
    modify (\s -> s { asCompiling = False })

run (Constant name) = do
  v <- pop
  create name $ InlineLit v

run (Variable name) = do
  p <- here
  create name $ InlineLit p
  comma 0

run OrgDirective = do
  a <- pop
  if a .&. 1 /= 0
    then throwError ("odd address in org directive: " ++ show a)
    else modify $ \s -> s { asPos = a `shiftR` 1 }

run (ALUPrim name) = do
  bits <- pop
  if 0 <= bits && bits < 65536
    then create name $ RawInst bits
    else throwError "ALU primitive must fit in 16 bits"

run (Interp (Comment _)) = pure ()

run (Interp (Word s)) = do
  d <- find s
  case d of
    InlineLit x -> push x
    _ -> throwError ("word " ++ s ++ " cannot be used in interpreted code")

run (Interp (Begin _ _)) = throwError "loops not permitted in interpreted code"
run (Interp (If _ _)) = throwError "if-then not permitted in interpreted code"

compile' :: AsmFrag -> Asm ()
compile' (Comment _) = pure ()
compile' (Word w) = find w >>= compile
compile' (Begin body end) = do
  begin <- here
  blockFusion
  mapM_ compile' body
  case end of
    Again -> jmp begin
    Until -> jmp0 begin

compile' (If trueBody melse) = do
  let placeholder = 0
      mark = here <* comma placeholder <* blockFusion
      resolve op loc = do
        dest <- here
        patch (Data placeholder) (Inst (op .|. dest)) loc
        blockFusion

  if_ <- mark
  mapM_ compile' trueBody

  case melse of
    Just falseBody -> do
      else_ <- mark
      resolve 0x2000 if_
      mapM_ compile' falseBody
      resolve 0x0000 else_

    Nothing -> resolve 0x2000 if_

compile :: Def -> Asm()
compile (Compiled a)
  | a < 8192 = cComma (0x4000 .|. a)
  | otherwise = error "internal error: colon def out of range"

compile (InlineLit x)
  | 0 <= x && x < 32768 = cComma (x .|. 0x8000)
  | otherwise = error "internal error: literal out of range"

compile (RawInst i)
  | 0 <= i && i < 65536 = cComma i
  | otherwise = error "internal error: instruction doesn't fit in 16 bits"

compile (Immediate f) = f

jmp, jmp0 :: Int -> Asm ()
jmp a | a < 8192 = cComma a
      | otherwise = error "internal error: jump destination out of range"

jmp0 a | a < 8192 = cComma $ 0x2000 .|. a
       | otherwise = error "internal error: jump destination out of range"

macroBang :: Asm ()
macroBang = compileOnly $ do
  cComma 0x6020   -- non-destructive store
  cComma 0x6103   -- drop
  cComma 0x6103   -- drop

compileOnly :: Asm () -> Asm ()
compileOnly x = do
  c <- gets asCompiling
  if c
    then x
    else throwError "use of compile-only word in interpreted context"

asm :: [AsmTop] -> Asm ()
asm tops = do
  create "exit" $ Immediate $ compileOnly exit
  create "!" $ Immediate macroBang
  forM_ tops run

main :: IO ()
main = do
  [source, dest] <- getArgs
  pr <- parseSourceFile source

  case pr of
    Left e -> print e
    Right tops -> do
      let (r, s) = runState (runExceptT $ runAsm $ asm tops) def
      case r of
        Left str -> putStrLn $ "Error: " ++ str
        Right () -> do
          let maxAddr = fromJust $ S.lookupMax $ M.keysSet $ asMem s

          putStrLn "ok"
          putStrLn $ "cells used: " ++ show (maxAddr + 1)

          forM_ [0 .. maxAddr] $ \a ->
            case M.lookup a (asMem s) of
              Just v  -> printf "  %04x %04x\n" (2 * a) (deval v)
              Nothing -> printf "  %04x ....\n" (2 * a)

          putStrLn "Symbols:"
          forM_ (M.toList $ asDict s) $ \(n, d) -> case d of
            Compiled a -> printf "  %04x %s\n" (2 * a) n
            _ -> pure ()

          out <- openFile dest WriteMode
          forM_ [0 .. maxAddr] $ \a ->
            hPrintf out "%04x\n" $ maybe 0xDEAD deval $ M.lookup a $ asMem s
          hClose out
