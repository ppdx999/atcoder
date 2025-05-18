{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints1 :: IO Int
ints1 =
  ints >>= \case
    [x1] -> return x1
    _ -> error "ints1: wrong number of integers"

solve :: [Int] -> [Int] -> Int
solve (a1 : as) bs = final
  where
    (final, _) = foldl step (a1, 0) (zip as bs)
    step (prev1, prev2) (a, b) = (min (prev1 + a) (prev2 + b), prev1)

main :: IO ()
main = do
  n <- ints1
  as <- ints
  bs <- ints
  print $ solve as bs