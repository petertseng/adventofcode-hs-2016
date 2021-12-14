import AdventOfCode (readInputFile)

import Data.List (dropWhileEnd, mapAccumL)
import qualified Data.Set as Set

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)
type Inst = ((Int, Int) -> (Int, Int), Int)

move :: (Pos, Dir) -> Inst -> ((Pos, Dir), [Pos])
move ((y, x), Dir (dy, dx)) (turn, dist) = ((pat dist, Dir (dy', dx')), [pat t | t <- [1 .. dist]])
  where (dy', dx') = turn (dy, dx)
        pat t = (y + dy' * t, x + dx' * t)

manhattan :: Pos -> Int
manhattan (y, x) = abs y + abs x

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = firstRepeat' Set.empty
  where firstRepeat' _ [] = Nothing
        firstRepeat' seen (x:_) | x `Set.member` seen = Just x
        firstRepeat' seen (x:xs) = firstRepeat' (Set.insert x seen) xs

inst :: String -> ((Int, Int) -> (Int, Int), Int)
inst s = case dropWhileEnd (== ',') s of
  -- Choose an arbitrary direction convention because it really doesn't matter.
  'R':n -> (\(dy, dx) -> (-dx, dy), read n)
  'L':n -> (\(dy, dx) -> (dx, -dy), read n)
  _ -> error ("bad dir " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let insts = map inst (words s)
      start = ((0, 0), Dir (1, 0))
  let ((endpos, _), poses) = mapAccumL move start insts

  print (manhattan endpos)
  putStrLn (maybe "no" (show . manhattan) (firstRepeat (concat poses)))
