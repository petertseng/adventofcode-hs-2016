import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

import Data.Array.Unboxed ((!), UArray, array)
import Data.Char (isDigit)
import Data.Either (rights)
import Data.List (partition, permutations)
import Data.Map (Map)
import qualified Data.Map as Map

type Pos = (Int, Int)

totalDist :: Map (Pos, Pos) Int -> [Pos] -> Int
totalDist _ [] = error "going nowhere???"
totalDist _ [_] = 0
totalDist dists (x1:x2:xs) = dists Map.! (x1, x2) + totalDist dists (x2:xs)

distsFrom :: UArray Pos Char -> Pos -> [((Pos, Pos), Int)]
distsFrom g start = map pair dists
  where dists = rights (bfs (neigh g) id (goal g) start)
        pair (dist, end) = ((start, end), dist)

neigh :: UArray Pos Char -> Pos -> [Pos]
neigh g (y, x) = filter ((/= '#') . (g !)) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

goal :: UArray Pos Char -> Pos -> Bool
goal g p = isDigit (g ! p)

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x c -> ((y, x), c)) [0..]

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

main :: IO ()
main = do
  s <- readInputFile
  let g = enumGrid (lines s)
      nums = filter (isDigit . snd) g
      height = length (lines s)
      width = uniform length (lines s)
      ga = array ((0, 0), (height - 1, width - 1)) g
      (zeroes, others) = partition ((== '0') . snd) nums
      start = case zeroes of
        [(st, _)] -> st
        [] -> error "no zeroes"
        _:_:_ -> error "too many zeroes"
      dists = Map.fromList (concatMap (distsFrom ga . fst) nums)
      paths = map (start :) (permutations (map fst others))
      cycles = map (++ [start]) paths
  print (minimum (map (totalDist dists) paths))
  print (minimum (map (totalDist dists) cycles))
