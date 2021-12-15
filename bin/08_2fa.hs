import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Foldable (for_)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)
data Inst = Rect Int Int | RotateRow Int Int | RotateCol Int Int

width :: Int
width = 50

height :: Int
height = 6

run :: Set Pos -> Inst -> Set Pos
run pts i = case i of
  Rect w h -> pts `Set.union` Set.fromList [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
  RotateRow y n -> Set.map (\(py, px) -> (py, if py == y then (px + n) `rem` width else px)) pts
  RotateCol x n -> Set.map (\(py, px) -> (if px == x then (py + n) `rem` height else py, px)) pts

inst :: String -> Inst
inst s = case words s of
  ["rect", n] -> uncurry Rect ((read *** read) (splitOnOne 'x' n))
  ["rotate", "row", 'y':'=':y, "by", n] -> RotateRow (read y) (read n)
  ["rotate", "column", 'x':'=':x, "by", n] -> RotateCol (read x) (read n)
  _ -> error ("bad inst " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let insts = map inst (lines s)
      pixels = foldl' run Set.empty insts
  print (Set.size pixels)

  let yr = [0 .. height - 1]
      xr = [0 .. width - 1]
  for_ yr (\y -> putStrLn (map (\x -> if (y, x) `Set.member` pixels then '#' else ' ') xr))
