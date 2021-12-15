import AdventOfCode (readInputFile)

import Data.List (dropWhileEnd, foldl')

type Disc = (Int, Int, Int)

dropTime :: [Disc] -> Int
dropTime = snd . foldl' addDisc (1, 0)
  where addDisc (step, t) (discId, positions, offset) =
          let (x, g) = gcdExtPos step positions
              initialDiff = -offset - discId - t :: Int
          in if initialDiff `mod` g /= 0 then error ("bad " ++ show (initialDiff, g, step, positions))
             else (lcm step positions, t + ((step * (((initialDiff `mod` positions) * x) `mod` positions)) `div` g))

gcdExtPos :: Integral a => a -> a -> (a, a)
gcdExtPos a n = let (x, _, g) = gcdExt a n in (mkPos x, g)
  where mkPos x | x < 0 = x + n
        mkPos x = x

-- https://rosettacode.org/wiki/Modular_inverse#Haskell
-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

disc :: String -> Disc
disc s = case words s of
  ["Disc", '#':n, "has", poses, "positions;", "at", "time=0,", "it", "is", "at", "position", p0] -> (read n, read poses, read (dropWhileEnd (== '.') p0))
  _ -> error ("bad disc " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let discs = map disc (lines s)
  print (dropTime discs)
  print (dropTime (discs ++ [(length discs + 1, 11, 0)]))
