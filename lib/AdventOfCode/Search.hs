{-# LANGUAGE TupleSections #-}

module AdventOfCode.Search (
  bfs,
) where

import qualified Data.Set as Set

-- so far in other years I've not had to use hash,
-- but it's useful in day 11 specifically
bfs :: (Ord a, Ord b) => (a -> [a]) -> (a -> b) -> (a -> Bool) -> a -> [Either Int (Int, a)]
bfs neigh hash goal start = bfs' 0 Set.empty (Set.singleton start)
  where bfs' gen _ s | Set.null s = [Left (gen - 1)]
        bfs' gen seen front = map (Right . (gen,)) goals ++ bfs' (gen + 1) seen' front'
          where goals = filter goal (Set.toList front)
                seen' = seen `Set.union` Set.map hash front
                front' = Set.fromList (concatMap (filter ((`Set.notMember` seen') . hash) . neigh) front)
