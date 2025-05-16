{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array (listArray, (!))
import Data.Array.IArray (Array, IArray (bounds), accumArray, array, assocs)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints1 :: IO Int
ints1 =
  ints >>= \case
    [x] -> return x
    _ -> error "int1: wrong number of integers"

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

ints3 :: IO (Int, Int, Int)
ints3 =
  ints >>= \case
    [x1, x2, x3] -> return (x1, x2, x3)
    _ -> error "ints3: wrong number of integers"

ints4 :: IO (Int, Int, Int, Int)
ints4 =
  ints >>= \case
    [x1, x2, x3, x4] -> return (x1, x2, x3, x4)
    _ -> error "ints4: wrong number of integers"

type Matrix = Array (Int, Int) Int

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

newMatrix :: ((Int, Int), (Int, Int)) -> [(Int, Int, Int, Int)] -> Matrix
newMatrix bound qs = accumArray (+) 0 bound $ concatMap parse qs
  where
    parse :: (Int, Int, Int, Int) -> [((Int, Int), Int)]
    parse (a, b, c, d) =
      [ ((a, b), 1),
        ((c + 1, b), -1),
        ((a, d + 1), -1),
        ((c + 1, d + 1), 1)
      ]

printMat :: ((Int, Int), (Int, Int)) -> Matrix -> IO ()
printMat bound mat = do
  let ((r0, c0), (r1, c1)) = bound
  mapM_
    putStrLn
    [ unwords [show (mat ! (i, j)) | j <- [c0 .. c1]]
      | i <- [r0 .. r1]
    ]

main :: IO ()
main = do
  (h, w, n) <- ints3
  qs <- replicateM n ints4
  let bound = ((0, 0), (h, w))
      idxVal (a, b, c, d) = [((a, b), 1), ((c + 1, b), -1), ((a, d + 1), -1), ((c + 1, d + 1), 1)]
      mat = accumArray (+) 0 bound $ concatMap idxVal qs :: Matrix
   in printMat ((1, 1), (h, w)) $ csum2D mat