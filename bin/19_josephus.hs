import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Char (isDigit)
import qualified Data.Sequence as Seq
import System.Environment (getArgs)

-- Intentionally not using the mathematical solutions,
-- to test out some data structures.

winner1Arr, winner1Seq, _w1 :: Int -> Int

-- Takes about 1s, okay (and fast to write) but not great.
winner1Seq n = winner1' (Seq.fromList [1 .. n])
  where
    winner1' s = case Seq.viewl s of
      Seq.EmptyL  -> error "no elves"
      e Seq.:< s' -> case Seq.viewl s' of
        Seq.EmptyL   -> e
        _ Seq.:< s'' -> winner1' (s'' Seq.|> e)
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
winner2Seq, winner2Arr, _w2 :: Int -> Int
winner2Seq n = winner2' (Seq.fromList [1 .. n])
  where
    winner2' s = case Seq.viewl s of
      Seq.EmptyL  -> error "no elves"
      e Seq.:< s'
        | Seq.null s' -> e
        | otherwise   -> winner2' (Seq.deleteAt ((Seq.length s' - 1) `div` 2) s' Seq.|> e)
_w2 = winner2Seq

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
    [] -> readFile "/dev/stdin" >>= (return . read)
    a:_ | all isDigit a -> return (read a)
    a:_ -> readFile a >>= (return . read)
  print (winner1Arr n)
  print (winner2Arr n)
