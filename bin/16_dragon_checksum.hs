import AdventOfCode.Split (splitOn)

import Data.Bits ((.&.), complement, popCount, shiftR, xor)
import Data.Foldable (for_)
import Data.List (isPrefixOf, mapAccumL, partition)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

disk :: [Bool] -> Int -> [Bool]
disk seed len = snd (mapAccumL (checksumBit seed (chunkSize len)) 0 [1 .. sumSize len])

checksumBit :: [Bool] -> Int -> Int -> Int -> (Int, Bool)
checksumBit seed csize ones chunk = (ones', (ones' `xor` ones) .&. 1 == 0)
  where dragons = chunklen `div` (seedlen + 1)
        (ab, prefix) = (chunklen - dragons) `quotRem` (seedlen * 2)
        -- ab * seedlen: odd only if both are odd, so can use .&. instead of *
        -- TODO: maybe can precompute the parity for each value of prefix
        ones' = dragonParity dragons + (ab .&. seedlen) + count id (take prefix (seed ++ map not (reverse seed)))
        chunklen = chunk * csize
        seedlen = length seed

dragonParity :: Int -> Int
dragonParity n = gray `xor` popCount (n .&. gray)
  where gray = n `xor` (n `shiftR` 1)

-- chunkSize: the largest power of 2 that divides disk.
-- e.g.   272 is 100010000
--        271 is 100001111
--       ~271 is  11110000
-- 272 & ~271 is     10000
chunkSize :: Int -> Int
chunkSize diskSize = diskSize .&. complement (diskSize - 1)

sumSize :: Int -> Int
sumSize diskSize = diskSize `quot` chunkSize diskSize

ctob :: Char -> Bool
ctob '0' = False
ctob '1' = True
ctob c = error (c : " bad bool")

btoc :: Bool -> Char
btoc b = if b then '1' else '0'

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  args <- getArgs
  let (flags, notFlags) = partition ("-" `isPrefixOf`) args
  input <- case notFlags of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let seed = map ctob input
      disks = case flags of
        ['-':'l':l] -> map read (splitOn ',' l)
        [] -> [272, 35651584]
        _ -> error ("bad flags " ++ show flags)

  for_ disks (putStrLn . map btoc . disk seed)
