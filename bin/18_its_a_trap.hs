import AdventOfCode.Split (splitOn)

import Data.Bits ((.&.), popCount, shiftL, shiftR, xor)
import Data.Foldable (for_)
import Data.List (dropWhileEnd, foldl', isPrefixOf, partition)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

nextRow :: Integer -> Integer -> Integer
nextRow mask x = ((x `shiftL` 1) `xor` (x `shiftR` 1)) .&. mask

trap :: Integer -> Char -> Integer
trap c '^' = c * 2 + 1
trap c '.' = c * 2
trap _ c = error (c : " bad char")

main :: IO ()
main = do
  args <- getArgs
  let (flags, notFlags) = partition ("-" `isPrefixOf`) args
  input <- case notFlags of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let bits = dropWhileEnd (== '\n') input
      len = length bits
      mask = (1 `shiftL` len) - 1

  let danger0 = foldl' trap 0 bits
      rows = iterate (nextRow mask) danger0
      nrows = case flags of
        ['-':'n':n] -> map read (splitOn ',' n)
        [] -> [40, 400000]
        _ -> error ("bad flags " ++ show flags)
  for_ nrows (print . sum . map ((len -) . popCount) . flip take rows)
