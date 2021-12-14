import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.List (zip4)

tls :: String -> Bool
tls s = hasAbba s && not (any hasAbba (snd (bracketed s)))

ssl :: String -> Bool
ssl s = any (\((a, b), (c, d)) -> a == d && b == c) [(a, b) | a <- concatMap abas unbracket, b <- concatMap abas bracket]
  where (unbracket, bracket) = bracketed s

bracketed :: String -> ([String], [String])
bracketed s = case splitOnOne '[' s of
  (l, "") -> ([l], [])
  (l, r) -> let (between, after) = splitOnOne ']' r in ((l:) *** (between:)) (bracketed after)

hasAbba :: Eq a => [a] -> Bool
hasAbba s = any abba (zip4 s (drop 1 s) (drop 2 s) (drop 3 s))
  where abba (a, b, c, d) = c == b && a == d && a /= b

abas :: String -> [(Char, Char)]
abas s = map (\(a, b, _) -> (a, b)) (filter aba (zip3 s (drop 1 s) (drop 2 s)))
  where aba (a, b, c) = a == c && a /= b

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  print (count tls (lines s))
  print (count ssl (lines s))
