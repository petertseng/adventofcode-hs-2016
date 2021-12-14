{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.List (maximumBy, minimumBy, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)

most :: Map a Int -> a
most = fst . maximumBy (comparing snd) . Map.assocs

least :: Map a Int -> a
least = fst . minimumBy (comparing snd) . Map.assocs

freqMap :: Ord a => [a] -> Map a Int
freqMap = Map.fromListWith (+) . map (, 1)

main :: IO ()
main = do
  s <- readInputFile
  let cols = transpose (lines s)
      freqs = map freqMap cols
  putStrLn (map most freqs)
  putStrLn (map least freqs)
