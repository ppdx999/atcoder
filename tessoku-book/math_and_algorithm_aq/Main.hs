module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List (unfoldr)
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

modPow :: Int -> Int -> Int -> Int
modPow a 0 m = 1
modPow a b m
  | even b = modPow ((a * a) `mod` m) (b `div` 2) m
  | otherwise = (a * modPow a (b - 1) m) `mod` m

main :: IO ()
main = do
  [a, b] <- ints
  print $ modPow a b 1000000007