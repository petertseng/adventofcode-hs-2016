import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, ord)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find)
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

zeroes :: String -> [(Int, Int)]
zeroes prefix = map (\h -> (b6 h, b7 h)) (filter (hasZeroes 5) (map (hashDoor prefix) [0..]))
  where b6 s = fromIntegral ((ByteString.unpack s !! 2) `rem` 16)
        b7 s = fromIntegral ((ByteString.unpack s !! 3) `quot` 16)

updatepw :: IntMap Char -> (Int, Int) -> IntMap Char
updatepw pw (pos, _) | pos < 0 || pos >= 8 = pw
updatepw pw (pos, c) = IntMap.insertWith (\_ old -> old) pos (hex c) pw

hashDoor :: String -> Int -> ByteString
hashDoor prefix = hash . BC.append (BC.pack prefix) . BC.pack . show

hex :: Int -> Char
hex n | 0 <= n && n <= 9 = chr (ord '0' + n)
hex n | 10 <= n && n <= 15 = chr (ord 'a' + n - 10)
hex _ = error "bad hex"

hasZeroes :: Int -> ByteString -> Bool
hasZeroes n s = fullZeroes && (even n || halfZero)
  where fullZeroes = all (== 0) (take half bytes)
        halfZero = bytes !! half < 16
        half = n `div` 2
        bytes = ByteString.unpack s

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  putStrLn (map (hex . fst) (take 8 (zeroes input)))
  let pw2 = scanl updatepw IntMap.empty (zeroes input)
      complete = fromJust (find ((== 8) . IntMap.size) pw2)
  putStrLn (IntMap.elems complete)
