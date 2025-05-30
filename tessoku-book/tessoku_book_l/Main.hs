{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

-- ///////////////////////////////////////////////////////////////////////
-- TEMPLATE
-- ///////////////////////////////////////////////////////////////////////

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

-- ///////////////////////////////////////////////////////////////////////
-- Main
-- ///////////////////////////////////////////////////////////////////////

check :: [Int] -> Int -> Int -> Bool
check as k n = k <= sum (map (n `div`) as)

binSearch :: [Int] -> Int -> Int -> Int -> Int
binSearch as ng ok k
  | ok - ng > 1 = binSearch as ng' ok' k
  | otherwise = ok
  where
    mid = (ok + ng) `div` 2
    (ng', ok') = if check as k mid then (ng, mid) else (mid, ok)

main :: IO ()
main = do
  (n, k) <- ints2
  as <- ints
  print $ binSearch as 0 1000000001 k