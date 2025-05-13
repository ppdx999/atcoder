module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import Distribution.Utils.Generic (safeHead)
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

boolYn True = "Yes"
boolYn False = "No"

moreThan n arr = length arr >= n

main :: IO ()
main = do
  (n : x : _) <- ints
  arr <- ints

  putStrLn $ boolYn $ moreThan 1 $ filter (== x) arr
