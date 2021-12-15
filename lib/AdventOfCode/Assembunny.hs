{-# LANGUAGE TupleSections #-}

module AdventOfCode.Assembunny (
  readProg,
  run,
  runA,
  zeroes,
) where

import Data.Array.IArray ((!), Array, bounds, inRange, listArray)
import Data.Char (isDigit, isLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Program = Array Int Inst
type Regs = Map Char Int
newtype Status = Finished Regs

data Inst = Copy Input Char | Increment Char | Decrement Char | JumpNotZero Input Input
data Input = Immed Int | Reg Char

zeroes :: [Char] -> Regs
zeroes = Map.fromList . map (, 0)

run :: Program -> Regs -> Status
run p = run' p 0

runA :: Program -> Regs -> Int
runA p r = case run' p 0 r of
  Finished r' -> r' Map.! 'a'

run' :: Program -> Int -> Regs -> Status
run' prog pc regs | not (bounds prog `inRange` pc) = Finished regs
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

checkPlusEquals :: Program -> Int -> Maybe (Char, Char)
checkPlusEquals prog pc | not (bounds prog `inRange` (pc + 2)) = Nothing
checkPlusEquals prog pc = case (prog ! pc, prog ! (pc + 1), prog ! (pc + 2)) of
  (Increment i, Decrement d, JumpNotZero (Reg j) (Immed (-2))) | i /= d && d == j -> Just (i, d)
  _ -> Nothing

resolve :: Regs -> Input -> Int
resolve regs i = case i of
  Immed v -> v
  Reg c -> regs Map.! c

readProg :: String -> Program
readProg s = listArray (0, length insts - 1) insts
  where insts = map inst (lines s)

inst :: String -> Inst
inst s = case words s of
  ["cpy", i, [d]] -> Copy (input i) d
  ["inc", [d]] -> Increment d
  ["dec", [d]] -> Decrement d
  ["jnz", i, d] -> JumpNotZero (input i) (input d)
  _ -> error ("bad inst " ++ s)

input :: String -> Input
input ('-':i) | all isDigit i = Immed (-read i)
input i | all isDigit i = Immed (read i)
input [i] | isLower i = Reg i
input s = error ("bad input " ++ s)
