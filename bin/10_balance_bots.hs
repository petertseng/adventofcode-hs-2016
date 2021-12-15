import AdventOfCode (readInputFile)

import Control.Arrow ((***), second)
import Data.Either (partitionEithers)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data Source = Bot Int ((Int, Int) -> Int) | Value Int

instance Show Source where
  show (Bot i f) = "Bot " ++ show i ++ " " ++ classify f
    where classify f' | f' (1, 2) == 2 = "high"
          classify f' | f' (1, 2) == 1 = "low"
          classify _ = "???"
  show (Value i) = "Value " ++ show i

-- Partially to see if it's possible,
-- and partially because I don't feel like doing the run-every-bot simulation,
-- let's try to run it in reverse,
-- where each bot looks at where it should be getting its outputs from.

botValues :: IntMap (Source, Source) -> IntMap (Int, Int)
botValues srcs = m
  where m = IntMap.map (resolveSource m *** resolveSource m) srcs

resolveSource :: IntMap (Int, Int) -> Source -> Int
resolveSource _ (Value n) = n
resolveSource m (Bot b f) = f (m IntMap.! b)

-- left outputs
-- right bots
dest :: String -> [Either (Int, Source) (Int, Source)]
dest s = case words s of
  ["value", n, "goes", "to", "bot", b] -> [Right (read b, Value (read n))]
  ["bot", b, "gives", "low", "to", d1, d2, "and", "high", "to", d3, d4] -> [dest' d1 d2 min, dest' d3 d4 max]
    where dest' t n f = botOrOut t (read n, Bot (read b) (uncurry f))
          botOrOut "bot" = Right
          botOrOut "output" = Left
          botOrOut _ = error ("bad dest type " ++ s)
  _ -> error ("bad dest " ++ s)

mapExactlyTwo :: [(Int, a)] -> IntMap (a, a)
mapExactlyTwo = IntMap.mapWithKey pair . list
  where list = IntMap.fromListWith (++) . map (second (:[]))
        pair _ [a, b] = (a, b)
        pair k [] = error ("zero for " ++ show k)
        pair k [_] = error ("one for " ++ show k)
        pair k (_:_:_:_) = error ("too many for " ++ show k)

one :: [a] -> a
one [x] = x
one [] = error "one of none"
one (_:_) = error "one of many"

main :: IO ()
main = do
  s <- readInputFile
  let (outs, bots) = partitionEithers (concatMap dest (lines s))
      botVals = botValues (mapExactlyTwo bots)
      (cmp1761, _) = one (filter ((\x -> x == (17, 61) || x == (61, 17)) . snd) (IntMap.assocs botVals))
  print cmp1761

  let outs012 = [one (filter ((== o) . fst) outs) | o <- [0, 1, 2]]
  print (product (map (resolveSource botVals . snd) outs012))
