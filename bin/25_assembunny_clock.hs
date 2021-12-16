import AdventOfCode (readInputFile)
import AdventOfCode.Assembunny (readProg, runOuts, zeroes)

import Data.Maybe (fromJust)
import Data.List (find)

import qualified Data.Map as Map

clock :: [Int]
clock = take 20 (cycle [0, 1])

main :: IO ()
main = do
  s <- readInputFile
  let prog = readProg s
      zs = zeroes "abcd"
      outs v = take 20 (runOuts prog (Map.insert 'a' v zs))
      good v = outs v == clock
  print (fromJust (find good [1 ..]))
