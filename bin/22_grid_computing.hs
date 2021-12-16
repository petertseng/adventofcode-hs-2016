import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOn)

import Data.Either (rights)
import Data.List (dropWhileEnd, isPrefixOf, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set

type Pos = (Int, Int)
type Node = (Pos, Int, Int)

compat :: Node -> Node -> Bool
compat (_, ua, _) (_, _, ab) = ua /= 0 && ua <= ab

adj4 :: Pos -> [Pos]
adj4 (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

node :: String -> Node
node s = case words s of
  [n, _, u, a, _] -> (nodePos n, readSz u, readSz a)
    where readSz = read . dropWhileEnd (== 'T')
  _ -> error ("bad node " ++ s)

nodePos :: String -> Pos
nodePos s = case splitOn '-' s of
  ["/dev/grid/node", 'x':x, 'y':y] -> (read x, read y)
  _ -> error ("bad node pos " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let nodes = map node (dropWhile (\l -> not ("/dev/grid/node-x" `isPrefixOf` l)) (lines s))
      compats = filter (uncurry compat) [(n1, n2) | n1 <- nodes, n2 <- nodes, n1 /= n2]
  print (length compats)

  let movables = Set.fromList (concatMap (\((a, _, _), (b, _, _)) -> [a, b]) compats)
      empty = mapMaybe (\(p, u, _) -> if u == 0 then Just p else Nothing) nodes
      neigh = filter (`Set.member` movables) . adj4
      bfs1 = take 1 . rights . bfs neigh id ((== 0) . snd)
      dataX = maximum (mapMaybe (\((x, y), _, _) -> if y == 0 then Just x else Nothing) nodes)
      (emptyToY0, (emptyX, _)) = minimumBy (comparing fst) (concatMap bfs1 empty)
      dataFirstStep = dataX - emptyX
      dataSubsequentSteps = (dataX - 1) * 5
  print (emptyToY0 + dataFirstStep + dataSubsequentSteps)
