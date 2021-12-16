import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (toList)
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Sequence ((|>), Seq)
import qualified Data.Sequence as Seq
import System.Directory (doesFileExist)
import System.Environment (getArgs)

-- Seq faster at appending to the end.

paths :: String -> [Seq Char]
paths prefix = walk prefix [(Seq.empty, (0, 0))]
--paths :: String -> [String]
--paths prefix = walk prefix [("", (0, 0))]

-- walk :: String -> [(String, (Int, Int))] -> [String]
walk :: String -> [(Seq Char, (Int, Int))] -> [Seq Char]
walk _ [] = []
walk prefix ends = map fst finished ++ walk prefix (concatMap (move prefix) unfinished)
  where (finished, unfinished) = partition ((== (3, 3)) . snd) ends

-- move :: String -> (String, (Int, Int)) -> [(String, (Int, Int))]
move :: String -> (Seq Char, (Int, Int)) -> [(Seq Char, (Int, Int))]
move prefix (path, (y, x)) = catMaybes [mu, md, ml, mr]
-- where (u, d, l, r) = hashPath prefix path
--        mv b1 b2 letter coord = if b1 && b2 then Just (path ++ [letter], coord) else Nothing
  where (u, d, l, r) = hashPath prefix (toList path)
        mv b1 b2 letter coord = if b1 && b2 then Just (path |> letter, coord) else Nothing
        mu = mv (y > 0) u 'U' (y - 1, x)
        md = mv (y < 3) d 'D' (y + 1, x)
        ml = mv (x > 0) l 'L' (y, x - 1)
        mr = mv (x < 3) r 'R' (y, x + 1)

hashPath :: String -> String -> (Bool, Bool, Bool, Bool)
hashPath prefix = openDoors . hash . BC.append (BC.pack prefix) . BC.pack

openDoors :: ByteString -> (Bool, Bool, Bool, Bool)
openDoors s = (open (byte0 `quot` 16), open (byte0 `rem` 16), open (byte1 `quot` 16), open (byte1 `rem` 16))
  where open x = x >= 11
        (byte0, byte1) = case ByteString.unpack s of
          a:b:_ -> (a, b)
          _ -> error "not enough bytes in hash"

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
--  putStrLn (head (paths input))
--  print (length (last (paths input)))
  putStrLn (toList (head (paths input)))
  print (Seq.length (last (paths input)))
