import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))

decompress :: Bool -> String -> Int
decompress _ [] = 0
decompress _ ['\n'] = 0
decompress recur ('(':xs) = expansion + decompress recur afterExpandee
  where (marker, afterParen) = splitOnOne ')' xs
        (len, mult) = (read *** read) (splitOnOne 'x' marker)
        (expandee, afterExpandee) = splitAt len afterParen
        expansion = mult * if recur then decompress recur expandee else len
decompress recur (c:xs) | 'A' <= c && c <= 'Z' = 1 + decompress recur xs
decompress _ s = error ("bad string " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  print (decompress False s)
  print (decompress True s)
