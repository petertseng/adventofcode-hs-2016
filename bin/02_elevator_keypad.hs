import AdventOfCode (readInputFile)

import Data.List (foldl', mapAccumL)

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)

pwchar :: (Pos -> Char) -> (Pos -> Bool) -> Pos -> [Dir] -> (Pos, Char)
pwchar key ok pos dirs = (pos', key pos')
  where pos' = foldl' (move ok) pos dirs

move :: (Pos -> Bool) -> Pos -> Dir -> Pos
move ok (y, x) (Dir (dy, dx)) = if ok p' then p' else (y, x)
  where p' = (y + dy, x + dx)

ok1 :: Pos -> Bool
ok1 (y, x) = 0 <= y && y <= 2 && 0 <= x && x <= 2

key1 :: Pos -> Char
key1 (0, 0) = '1'
key1 (0, 1) = '2'
key1 (0, 2) = '3'
key1 (1, 0) = '4'
key1 (1, 1) = '5'
key1 (1, 2) = '6'
key1 (2, 0) = '7'
key1 (2, 1) = '8'
key1 (2, 2) = '9'
key1 s = error ("bad pos " ++ show s)

ok2 :: Pos -> Bool
ok2 (y, x) = abs y + abs x <= 2

key2 :: Pos -> Char
key2 (-2, 0) = '1'
key2 (-1, -1) = '2'
key2 (-1, 0) = '3'
key2 (-1, 1) = '4'
key2 (0, -2) = '5'
key2 (0, -1) = '6'
key2 (0, 0) = '7'
key2 (0, 1) = '8'
key2 (0, 2) = '9'
key2 (1, -1) = 'A'
key2 (1, 0) = 'B'
key2 (1, 1) = 'C'
key2 (2, 0) = 'D'
key2 s = error ("bad pos " ++ show s)

inst :: String -> [Dir]
inst = map dir
  where dir 'U' = Dir (-1, 0)
        dir 'D' = Dir (1, 0)
        dir 'L' = Dir (0, -1)
        dir 'R' = Dir (0, 1)
        dir c = error (c : " bad dir")

main :: IO ()
main = do
  s <- readInputFile
  let insts = map inst (lines s)
  putStrLn (snd (mapAccumL (pwchar key1 ok1) (1, 1) insts))
  putStrLn (snd (mapAccumL (pwchar key2 ok2) (0, -2) insts))
