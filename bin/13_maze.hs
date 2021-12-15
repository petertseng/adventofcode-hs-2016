import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Bits (popCount)
import Data.Char (isDigit)
import Data.Either (fromRight, rights)
import Data.List (isPrefixOf, partition)
import System.Environment (getArgs)

neigh :: Int -> (Int, Int) -> [(Int, Int)]
neigh n (x, y) = filter (\nn -> inBounds nn && open n nn) cands
  where cands = [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
        inBounds (nx, ny) = nx >= 0 && ny >= 0

open :: Int -> (Int, Int) -> Bool
open n (x, y) = even (popCount (x * x + 3 * x + 2 * x * y + y + y * y + n))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  args <- getArgs
  let (flags, notFlags) = partition ("-" `isPrefixOf`) args
  n <- case notFlags of
    [] -> fmap read (readFile "/dev/stdin")
    a:_ | all isDigit a -> return (read a)
    a:_ -> fmap read (readFile a)
  let goal = case flags of
        ['-':'g':g] -> (read *** read) (splitOnOne ',' g)
        [] -> (31, 39)
        _ -> error ("bad flags " ++ show flags)
      steps = bfs (neigh n) id (== goal) (1, 1)
  print (fst (fromRight (error "not found") (head steps)))

  let poses = rights (bfs (neigh n) id (const True) (1, 1))
  print (count ((<= 50) . fst) poses)
