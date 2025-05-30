{-# LANGUAGE LambdaCase #-}

import Control.Monad (replicateM)
import Data.Array (Array, accumArray, assocs, bounds, listArray, (!))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Functor ((<&>))

ints :: IO [Int]
ints = map read . words <$> getLine

ints4 = ints <&> (\[x1, x2, x3, x4] -> (x1, x2, x3, x4))

type Matrix = Array (Int, Int) Int

-- | 二次元累積和を計算する
csum2D :: Matrix -> Matrix
csum2D mat = dp
  where
    dp = listArray (bounds mat) (go <$> assocs mat)
    go = \case
      (ij@(i, j), a)
        | ij == ij0 -> a
        | i == i0 -> (dp ! pred_j) + a
        | j == j0 -> (dp ! pred_i) + a
        | otherwise -> (dp ! pred_i) + (dp ! pred_j) - (dp ! pred_ij) + a
        where
          ij0 = fst (bounds mat)
          i0 = fst ij0
          j0 = snd ij0
          pred_i = first pred ij
          pred_j = second pred ij
          pred_ij = bimap pred pred ij

solve h w n qs =
  -- 累積和を計算する
  csum2D $
    -- [(a,b,c,d)]の各要素の情報をもとに適切に+1,-1した二次元配列を作る
    accumArray (+) 0 ((0, 0), (h + 1, w + 1)) $
      concatMap
        (\(a, b, c, d) -> [((a, b), 1), ((c + 1, b), -1), ((a, d + 1), -1), ((c + 1, d + 1), 1)])
        qs

printMat bound mat = do
  let ((r0, c0), (r1, c1)) = bound
  mapM_
    putStrLn
    [ unwords [show (mat ! (i, j)) | j <- [c0 .. c1]]
      | i <- [r0 .. r1]
    ]

main :: IO ()
main = do
  -- 入力
  [h, w, n] <- ints
  qs <- replicateM n ints4
  -- 出力
  printMat ((1, 1), (h, w)) $ solve h w n qs
