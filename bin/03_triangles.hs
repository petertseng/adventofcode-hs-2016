import AdventOfCode (readInputFile)

import Data.List (sort, transpose)

triangle :: [Int] -> Bool
triangle xs = case sort xs of
  [a, b, c] -> a + b > c
  _ -> error ("not three " ++ show xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

every3 :: [a] -> [[a]]
every3 (a:b:c:xs) = [a, b, c] : every3 xs
every3 [] = []
every3 [_] = error "one element left"
every3 [_,_] = error "two elements left"

main :: IO ()
main = do
  s <- readInputFile
  let nums = map (map read . words) (lines s)
  print (count triangle nums)
  let transposed = concatMap every3 (transpose nums)
  print (count triangle transposed)
