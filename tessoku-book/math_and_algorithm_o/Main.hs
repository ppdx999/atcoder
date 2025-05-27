module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)
import Prelude hiding (gcd)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

gcd :: (Integral a) => a -> a -> a
gcd a 0 = abs a
gcd a b = gcd b (a `mod` b)

main :: IO ()
main = do
  [a, b] <- ints
  print $ gcd a b