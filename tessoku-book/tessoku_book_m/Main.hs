{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

type Index = Int

type Value = Int

shakutori :: [Int] -> Int -> Int
shakutori as k = go pairs pairs
  where
    n = length as
    pairs = zip [0 ..] as

    go :: [(Index, Value)] -> [(Index, Value)] -> Int
    -- 左端に到達したら完全終了
    go [] _ = 0
    -- 右端に到達したらLを１進める
    -- このときNとposLの差分が条件を満たす組み合わせの数になる
    go lls@((posL, _) : ls) [] = (n - posL - 1) + go ls []
    go lls@((posL, l) : ls) rrs@((posR, r) : rs)
      -- 同じ場所に来たらRを一つ進める
      | posL == posR = go lls rs
      -- 条件を満たすならRを一つ進める
      | r - l <= k = go lls rs
      -- 条件を満たさないならLを進める
      -- このときposRとposLの差分が条件を満たす組み合わせの数になる
      | otherwise = (posR - posL - 1) + go ls rrs

main :: IO ()
main = do
  (n, k) <- ints2
  as <- ints
  print $ shakutori as k