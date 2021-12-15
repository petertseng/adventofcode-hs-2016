import AdventOfCode (readInputFile)
import AdventOfCode.Assembunny (readProg, runA, zeroes)

import qualified Data.Map as Map

main :: IO ()
main = do
  s <- readInputFile
  let prog = readProg s
      zs = zeroes "abcd"
  print (runA prog zs)
  print (runA prog (Map.insert 'c' 1 zs))
