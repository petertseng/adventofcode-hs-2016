import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Data.List (sort)

type Range = (Int, Int)

merge :: [Range] -> [Range]
merge = mergeSorted . sort

mergeSorted :: [Range] -> [Range]
mergeSorted [] = []
mergeSorted ((min1, max1):(min2, max2):xs) | succ max1 >= min2 = mergeSorted ((min1, max max1 max2) : xs)
mergeSorted (x:xs) = x : mergeSorted xs

intervalSize :: Range -> Int
intervalSize (a, b) = b - a + 1

rule :: String -> Range
rule = tmap read . splitOnOne '-'
  where tmap f (a, b) = (f a, f b)

main :: IO ()
main = do
  s <- readInputFile
  let rules = map rule (lines s)
      merged = merge rules
      minUnblocked = case merged of
        (0, mx):_ -> mx + 1
        []  -> 0
        _:_ -> 0
  print minUnblocked
  print ((2 ^ (32 :: Int)) - sum (map intervalSize merged))
