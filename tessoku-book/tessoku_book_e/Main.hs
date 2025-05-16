module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

within :: Int -> Int -> [Int] -> [Int]
within l r = filter (\x -> l <= x && x <= r)

main :: IO ()
main = do
  (n : k : _) <- ints
  let red = [1 .. n]
      blue = [1 .. n]
      possibleAnswers = [k - x - y | x <- red, y <- blue]
      countUpAnswer = length . within 1 n
  print $ countUpAnswer possibleAnswers