import AdventOfCode (readInputFile)
import AdventOfCode.Assembunny (readProg, runA, zeroes)

import qualified Data.Map as Map

factorial :: Int -> Int
factorial n = product [1 .. n]

main :: IO ()
main = do
  s <- readInputFile
  let prog = readProg s
      zs = zeroes "abcd"
      a v = runA prog (Map.insert 'a' v zs)
  let a6 = a 6
      a7 = a 7
      a8 = a 8
  case (a7 - a6, a8 - a7) of
    (0, 0) -> print a7 >> print (a 12)
    (4320, 35280) -> print a7 >> print (a7 + factorial 12 - factorial 7)
    _ -> error ("unrecognised differences between a=6, a=7, a=8" ++ show (a6, a7, a8))
