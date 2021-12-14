{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.Array.Unboxed (UArray, accumArray, assocs)
import Data.Char (chr, isDigit, ord)
import Data.Foldable (for_)
import Data.List (isInfixOf, isSuffixOf, sortOn)
import Data.Ord (Down(Down))

realRoom :: String -> Bool
realRoom s = ('[' : checksum s ++ "]") `isSuffixOf` s

encName :: String -> String
encName = takeWhile (not . isDigit)

sectorID :: String -> Int
sectorID = read . filter isDigit

checksum :: String -> String
checksum s = map fst (take 5 sorted)
  where sorted = sortOn (\(a, b) -> (Down b, a)) (assocs tally)
        tally = freqMap (filter (/= '-') (encName s))

decrypt :: String -> String
decrypt s = map (rotate (sectorID s)) (encName s) ++ show (sectorID s)

rotate :: Int -> Char -> Char
rotate n c | 'a' <= c && c <= 'z' = chr (((ord c - ord 'a' + n) `rem` 26) + ord 'a')
rotate _ c = c

freqMap :: [Char] -> UArray Char Int
freqMap = accumArray (+) 0 ('a', 'z') . map (, 1)

main :: IO ()
main = do
  s <- readInputFile
  let rooms = lines s
      realRooms = filter realRoom rooms
  print (sum (map sectorID realRooms))
  let decrypted = map decrypt realRooms
  for_ (filter ("north" `isInfixOf`) decrypted) putStrLn
