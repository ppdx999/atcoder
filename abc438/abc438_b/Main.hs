{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List (unfoldr, tails)
import GHC.Unicode (isSpace)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

-- ///////////////////////////////////////////////////////////////////////
-- Main
-- ///////////////////////////////////////////////////////////////////////


diff a b = (fromEnum a - fromEnum b) `mod` 10

cost ss' ts = sum $ zipWith diff ss' ts

substrings x n = [take n xs | xs <- tails x, length xs >= n]

solve ss ts = 
	let n = length ts
	    subs = substrings ss n
	in minimum [ sum (zipWith diff sub ts) | sub <- subs ]

main :: IO ()
main = do
  -- 入力
  (n,m) <- ints2
  ss <- getLine
  ts <- getLine
  -- 計算
  let ans = solve ss ts
  -- 出力
  print ans
