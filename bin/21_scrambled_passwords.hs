import AdventOfCode (readInputFile)

import Data.List (foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

data Op = SwapPos Int Int
        | SwapLetter Char Char
        | Rotate Int
        | RotateBasedOnPos Char
        | Reverse Int Int
        | Move Int Int deriving Show

run :: Bool -> Map Char Int -> Op -> Map Char Int
run rev m o = case o of
  SwapPos x y -> Map.map (swap x y) m
  SwapLetter x y -> Map.mapKeys (swap x y) m
  Rotate i -> rotate (if rev then -i else i) m
  RotateBasedOnPos c ->
    let pos = m Map.! c
        -- 0 1 1
        -- 1 2 3
        -- 2 3 5
        -- 3 4 7
        -- 4 6 2
        -- 5 7 4
        -- 6 8 6
        -- 7 9 0
        r = if rev then -((pos `quot` 2) + if odd pos || pos == 0 then 1 else 5)
            else pos + 1 + (if pos >= 4 then 1 else 0)
    in rotate r m
  Reverse x y -> Map.map (\p -> if x <= p && p <= y then x + y - p else p) m
  Move x y -> Map.map (if rev then move y x else move x y) m

move :: Int -> Int -> Int -> Int
move x y p | p == x = y
move x y p = p + dx + dy
  where dx = if p >= x then -1 else 0
        dy = if p + dx >= y then 1 else 0

rotate :: Int -> Map Char Int -> Map Char Int
rotate i m = Map.map (\x -> (x + i) `mod` sz) m
  where sz = Map.size m

swap :: Eq a => a -> a -> a -> a
swap a b x | x == a = b | x == b = a | otherwise = x

op :: String -> Op
op s = case words s of
  ["swap", "position", x, "with", "position", y] -> SwapPos (read x) (read y)
  ["swap", "letter", [x], "with", "letter", [y]] -> SwapLetter x y
  ["rotate", "left", "1", "step"] -> Rotate (-1)
  ["rotate", "left", x, "steps"] -> Rotate (-read x)
  ["rotate", "right", "1", "step"] -> Rotate 1
  ["rotate", "right", x, "steps"] -> Rotate (read x)
  ["rotate", "based", "on", "position", "of", "letter", [x]] -> RotateBasedOnPos x
  ["reverse", "positions", x, "through", y] -> Reverse (read x) (read y)
  ["move", "position", x, "to", "position", y] -> Move (read x) (read y)
  _ -> error ("bad op " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let ops = map op (lines s)
      pw x = Map.fromAscList (zip x [0..])
      showpw = map fst . sortOn snd . Map.assocs
  putStrLn (showpw (foldl' (run False) (pw "abcdefgh") ops))
  putStrLn (showpw (foldr (flip (run True)) (pw "fbgdceah") ops))
