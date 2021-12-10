{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOn)

import Control.Arrow ((***), second)
import Control.Monad (unless, when)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (dropWhileEnd, foldl', partition, sort, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (floor)

data ItemType = Generator | Chip deriving (Eq, Ord, Show)
type Item a = (a, ItemType)
type State a = (Int, Map (Item a) Int)

-- This is dubious. There's no guarantee the special cases in it will handle all possible inputs.
cost :: [(Int, Int)] -> Int
cost [] = error "empty cost"
cost [_] = 0
cost ((c1, g1):(c2, g2):xs) = baseCost (c1 + g1) + penalty + cost ((c1 + c2, g1 + g2):xs)
  where baseCost 1 = 1
        baseCost n = 1 + 2 * (n - 2)
        -- c1 >= 2 && g1 == 0 && g2 == 1:
        -- You can't move both chips up at once. Only one. So penalty of 2 moves.
        -- g2 == 1 because if there were two generators on the target floor and they were of the correct types, you could.
        -- g1 == 0 because if there were a generator on the source floor, you could just move the generator.
        --
        -- c2 > 2:
        -- You can't move two generators up because that would fry the chips.
        -- You'll have to move the one microchip up and rearrange things from there.
        -- if c2 <= 2, you could move the right generators and pair them with the chips.
        -- You would think there should be an && c1 == 1 in there,
        -- reasoning that you could move two chips up, but apparently you should still be penalised.
        -- TODO: Explanation for why no c1 == 1? What happens if you move those two microchips up?
        -- TODO: tested this for c2 = 3 and c2 = 4... larger values TBD (trouble finding one that's actually solvable)
        --
        -- Note that both special cases can't be true at the same time since c2 > 2 implies there are chips,
        -- and g2 == 1 implies there's only one generator, so the chips would fry.
        penalty | c1 >= 2 && g1 == 0 && g2 == 1 = 2 | c2 > 2 = 2 * (c2 - 1) | otherwise = 0

neigh :: Ord a => Int -> State a -> [State a]
neigh topFloor (floor, items) = mapMaybe moveIfLegal [(out, target) | out <- itemsToMove (byFloor IntMap.! floor), target <- floors]
  where floors | floor == 0 = [1] | floor == topFloor = [floor - 1] | otherwise = [floor - 1, floor + 1]
        byFloor = itemsByFloor items
        moveIfLegal (moved, dest) = if legalFloor destItems' then Just (dest, items') else Nothing
          where destItems = IntMap.findWithDefault Set.empty dest byFloor
                destItems' = destItems `Set.union` Set.fromList moved
                items' = foldl' (\m it -> Map.insert it dest m) items moved

itemsToMove :: Ord a => Set (Item a) -> [[Item a]]
itemsToMove items = filter legalWithout (pairs ++ map (:[]) singles)
  where pairs = [[x, y] | x:xs <- tails singles, y <- xs]
        singles = Set.toList items
        legalWithout removed = legalFloor (items `Set.difference` Set.fromList removed)

legalFloor :: Ord a => Set (Item a) -> Bool
legalFloor items = all ok items
  where noGen = all ((== Chip) . snd) items
        ok (_, Generator) = True
        ok (a, Chip) = noGen || (a, Generator) `Set.member` items

sortedPairs :: Ord a => [(Item a, Int)] -> [(Int, Int)]
sortedPairs = pairUp . sort
  where pairUp [] = []
        pairUp (((i, Generator), fg):((j, Chip), fc):xs) | i == j = (fg, fc) : pairUp xs
        pairUp _ = error "bad sortedPairs"

itemsByFloor :: Ord a => Map (Item a) Int -> IntMap (Set (Item a))
itemsByFloor = IntMap.fromListWith Set.union . map (\(i, f) -> (f, Set.singleton i)) . Map.assocs

mapFloors :: Ord a => [[Item a]] -> Map (Item a) Int
mapFloors floors = Map.fromList (concatMap labelFloor (zip [0..] floors))
  where labelFloor (n, items) = map (,n) items

countItems :: [Item a] -> (Int, Int)
countItems = (length *** length) . partition ((== Chip) . snd)

parseFloor :: String -> [Item String]
parseFloor = concatMap (parseItems . reverse . words) . splitOn ',' . dropWhileEnd (== '.')

parseItems :: [String] -> [Item String]
parseItems ["relevant", "nothing", "contains", "floor", _, "The"] = []
parseItems (a:b:"a":"and":x) = parseItems [a, b] ++ parseItems x
parseItems ("generator":x:_) = [(x, Generator)]
parseItems ("microchip":x:_) = [(takeWhile (/= '-') x, Chip)]
parseItems [] = []
parseItems s = error ("bad " ++ unwords (reverse s))

--alloc :: Map String Int -> String -> (Map String Int, Int)
--alloc m s = case Map.lookup s m of
  --Just i -> (m, i)
  --Nothing -> let i = Map.size m in (Map.insert s i m, i)

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let items = map parseFloor (lines s)
      counts = map countItems items
      m = mapFloors items
      state = (0, m)
      topFloor = length items - 1
      goal = (topFloor, Map.map (const topFloor) m)

  -- gives correct answer, but slow... may need to invesstigate how to make faster.
  when False $ print (bfs (neigh topFloor) (second (sortedPairs . Map.assocs)) (== goal) state)

  print (cost counts)

  let counts' = case counts of
        (c1, g1):cs -> (c1 + 2, g1 + 2) : cs
        [] -> [(2, 2)]
  unless (any ((== '1') . fst) flags) $ print (cost counts')
