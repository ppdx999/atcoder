{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array (listArray, (!))
import Data.Array.IArray (Array, IArray (bounds), array, assocs)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints4 = ints <&> (\[x1, x2, x3, x4] -> (x1, x2, x3, x4))

type Matrix = Array (Int, Int) Int

-- | 二次元配列を入力から読み込む
getMat :: Int -> Int -> IO Matrix
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

-- | 二次元累積和を計算する
csum2D :: Matrix -> Matrix
csum2D mat = dp
  where
    dp = listArray (bounds mat) (go <$> assocs mat)
    go = \case
      (ij@(i, j), a)
        | ij == ij0 -> a
        | i == i0 -> (dp ! pred_j ij) + a
        | j == j0 -> (dp ! pred_i ij) + a
        | otherwise -> (dp ! pred_i ij) + (dp ! pred_j ij) - (dp ! pred_ij ij) + a
        where
          ij0 = fst (bounds mat)
          i0 = fst ij0
          j0 = snd ij0
          pred_i = first pred
          pred_j = second pred
          pred_ij = bimap pred pred

-- | 0行目を追加 && 各行の先頭に0を追加
to1Index :: Matrix -> Matrix
to1Index mat = array ((r0, c0), (h, w)) newAssocs
  where
    ((r0, c0), (r1, c1)) = bounds mat
    h = r1 - r0 + 1
    w = c1 - c0 + 1

    newAssocs =
      -- line zero
      [((0, j), 0) | j <- [0 .. w]]
        -- first elem of each line
        ++ [((i, 0), 0) | i <- [1 .. h]]
        -- body elements
        ++ [((i, j), mat ! (i - 1, j - 1)) | i <- [1 .. h], j <- [1 .. w]]

solve h w mat qs = answers
  where
    answers = map calcAnswer qs

    -- 二次元累積和を取る
    cumMatrix = csum2D $ to1Index mat

    -- 二次元累積和を使って答えを求める関数
    calcAnswer (a, b, c, d) =
      (cumMatrix ! (a - 1, b - 1)) + (cumMatrix ! (c, d)) - (cumMatrix ! (a - 1, d)) - (cumMatrix ! (c, b - 1))

main :: IO ()
main = do
  -- 入力
  [h, w] <- ints
  mat <- getMat h w
  [q] <- ints
  qs <- replicateM q ints4
  -- 出力
  mapM_ print $ solve h w mat qs
