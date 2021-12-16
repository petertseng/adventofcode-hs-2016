{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCode.Assembunny (
  readProg,
  run,
  runA,
  runOuts,
  zeroes,
) where

import Data.Array.IArray ((!), (//), Array, bounds, inRange, listArray)
import Data.Char (isDigit, isLower)
import Data.List (unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Program = Array Int Inst
type Regs = Map Char Int
data Status = Finished Regs | Outputting (Int, () -> Status)

data Inst = Copy Input Char | Increment Char | Decrement Char | JumpNotZero Input Input | Toggle Input | NopCopy Input Int | Nop1 | Output Input
data Input = Immed Int | Reg Char

zeroes :: [Char] -> Regs
zeroes = Map.fromList . map (, 0)

run :: Program -> Regs -> Status
run p = run' p 0

runA :: Program -> Regs -> Int
runA p r = case run' p 0 r of
  Finished r' -> r' Map.! 'a'
  Outputting _ -> error "outputting"

runOuts :: Program -> Regs -> [Int]
runOuts p regs = unfoldr takeOut (\() -> run' p 0 regs)
  where takeOut f = case f () of
          Outputting (i, f') -> Just (i, f')
          Finished _ -> Nothing

run' :: Program -> Int -> Regs -> Status
run' prog pc regs | not (bounds prog `inRange` pc) = Finished regs
run' prog pc regs | Output o <- prog ! pc = Outputting (resolve regs o, \() -> run' prog (pc + 1) regs)
run' prog pc regs | Toggle t <- prog ! pc =
  let target = pc + resolve regs t
      prog' = if bounds prog `inRange` target then prog // [(target, toggle (prog ! target))] else prog
  in run' prog' (pc + 1) regs
run' prog pc regs = run' prog pc' regs'
  where
    (pc', regs') = case prog ! pc of
      Copy i d -> (pc + 1, Map.insert d (resolve regs i) regs)
      Increment c -> case checkPlusEquals prog pc of
        Nothing -> (pc + 1, Map.adjust succ c regs)
        Just (i, d) -> (pc + 3, Map.adjust (+ regs Map.! d) i (Map.insert d 0 regs))
      Decrement c -> (pc + 1, Map.adjust pred c regs)
      JumpNotZero i _ | resolve regs i == 0 -> (pc + 1, regs)
      JumpNotZero _ d -> (pc + resolve regs d, regs)
      Toggle _ -> error "toggle should have been handled in other case"
      NopCopy _ _ -> (pc + 1, regs)
      Nop1 -> (pc + 1, regs)
      Output _ -> error "out should have been handled in other case"

checkPlusEquals :: Program -> Int -> Maybe (Char, Char)
checkPlusEquals prog pc | not (bounds prog `inRange` (pc + 2)) = Nothing
checkPlusEquals prog pc = case (prog ! pc, prog ! (pc + 1), prog ! (pc + 2)) of
  (Increment i, Decrement d, JumpNotZero (Reg j) (Immed (-2))) | i /= d && d == j -> Just (i, d)
  _ -> Nothing

resolve :: Regs -> Input -> Int
resolve regs i = case i of
  Immed v -> v
  Reg c -> regs Map.! c

toggle :: Inst -> Inst
toggle (Copy i d) = JumpNotZero i (Reg d)
toggle (Increment c) = Decrement c
toggle (Decrement c) = Increment c
toggle (JumpNotZero i (Reg d)) = Copy i d
toggle (JumpNotZero i (Immed d)) = NopCopy i d
toggle (Toggle (Reg c)) = Increment c
toggle (Toggle (Immed _)) = Nop1
toggle (NopCopy i d) = JumpNotZero i (Immed d)
toggle Nop1 = Nop1
-- by the rules of 23 it should become inc, but I'd rather forbid.
toggle (Output _) = error "toggle output"

readProg :: String -> Program
readProg s = listArray (0, length insts - 1) insts
  where insts = map inst (lines s)

inst :: String -> Inst
inst s = case words s of
  ["cpy", i, [d]] -> Copy (input i) d
  ["inc", [d]] -> Increment d
  ["dec", [d]] -> Decrement d
  ["jnz", i, d] -> JumpNotZero (input i) (input d)
  ["tgl", i] -> Toggle (input i)
  ["out", i] -> Output (input i)
  _ -> error ("bad inst " ++ s)

input :: String -> Input
input ('-':i) | all isDigit i = Immed (-read i)
input i | all isDigit i = Immed (read i)
input [i] | isLower i = Reg i
input s = error ("bad input " ++ s)
