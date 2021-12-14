{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.Array.Unboxed (Ix, UArray, accumArray, assocs)
import Data.List (maximumBy, minimumBy, transpose)
import Data.Ord (comparing)

most :: Ix a => UArray a Int -> a
most = fst . maximumBy (comparing snd) . assocs

least :: Ix a => UArray a Int -> a
least = fst . minimumBy (comparing snd) . filter ((> 0) . snd) . assocs

freqMap :: [Char] -> UArray Char Int
freqMap = accumArray (+) 0 ('a', 'z') . map (, 1)

main :: IO ()
main = do
  s <- readInputFile
  let cols = transpose (lines s)
      freqs = map freqMap cols
  putStrLn (map most freqs)
  putStrLn (map least freqs)
