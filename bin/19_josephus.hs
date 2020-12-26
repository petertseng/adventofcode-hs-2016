import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Char (isDigit)
import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as Seq
import System.Environment (getArgs)

-- Intentionally not using the mathematical solutions,
-- to test out some data structures.

winner1Arr, winner1Seq, _w1 :: Int -> Int

-- Takes about 1s, okay (and fast to write) but not great.
winner1Seq n = winner1' (Seq.fromList [1 .. n])
  where
    winner1' Seq.Empty = error "no elves"
    winner1' (l :<| Seq.Empty) = l
    winner1' (l :<| _ :<| s) = winner1' (s |> l)
_w1 = winner1Seq

-- Much faster than winner1Seq, about 10x difference
winner1Arr n = runST $ do
  right <- newListArray (1, n) ([2 .. n] ++ [1]) :: ST s (STUArray s Int Int)

  foldM (\current _ -> do
      victim <- readArray right current
      next <- readArray right victim
      writeArray right current next
      return next
    ) 1 [1 .. n - 1]

-- Takes about 3s, okay (and fast to write) but not great.
winner2Seq, winner2TwoSeq, winner2Arr, _w2, _w22 :: Int -> Int
winner2Seq n = winner2' (Seq.fromList [1 .. n])
  where
    winner2' Seq.Empty = error "no elves"
    winner2' (l :<| Seq.Empty) = l
    winner2' (l :<| s) = winner2' (s' |> l)
      where s' = Seq.deleteAt ((Seq.length s - 1) `div` 2) s
_w2 = winner2Seq

-- two seqs.
-- Faster than winner2Seq by about 2x.
-- lengths are either (n, n) or (n, n + 1)
-- 1 2 3 / 4 5 6 -> 2 3 / 5 6 1
-- 1 2   / 3 4 5 -> 2 4 / 5 1
-- 1 2   / 3 4   -> 2   / 4 1
-- 1     / 2 3   -> 3   / 1
-- 3     / 1     ->     / 3
--
-- l ls / _ rs   / even -> ls   / rs l
-- l ls / _ r rs / odd  -> ls r / rs l
winner2TwoSeq n = winner2' (Seq.splitAt (n `quot` 2) (Seq.fromList [1 .. n]))
  where
    winner2' (Seq.Empty, Seq.Empty) = error "no elves"
    winner2' (l :<| ls , _ :<| rs) | Seq.length ls == Seq.length rs = winner2' (ls, rs |> l)
    winner2' (l :<| ls , _ :<| r :<| rs) = winner2' (ls |> r, rs |> l)
    winner2' (Seq.Empty, r :<| Seq.Empty) = r
    winner2' _ = error "impossible"
_w22 = winner2TwoSeq

-- Much faster than winner2Seq, about 10x difference
winner2Arr n = fst $ runST $ do
  right <- newListArray (1, n) ([2 .. n] ++ [1]) :: ST s (STUArray s Int Int)

  foldM (\(current, beforeVictim) n' -> do
      victim <- readArray right beforeVictim
      afterVictim <- readArray right victim
      writeArray right beforeVictim afterVictim
      next <- readArray right current
      return (next, if odd n' then beforeVictim else afterVictim)
    ) (1, n `div` 2) [n - 1, n - 2 .. 1]

main :: IO ()
main = do
  args <- getArgs
  n <- case args of
    [] -> fmap read (readFile "/dev/stdin")
    a:_ | all isDigit a -> return (read a)
    a:_ -> fmap read (readFile a)
  print (winner1Arr n)
  print (winner2Arr n)
