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

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

type Matrix = Array (Int, Int) Int

getMat :: Int -> Int -> IO Matrix
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

scanMatrix :: (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int -> Int -> Int -> Int) -> Matrix -> Matrix
scanMatrix topLeftFn leftFn topFn othersFn mat = dp
  where
    dp = listArray (bounds mat) (go <$> assocs mat)
    go = \case
      (ij@(i, j), a)
        | ij == ij0 -> topLeftFn a
        | i == i0 -> topFn (dp ! pred_j ij) a
        | j == j0 -> leftFn (dp ! pred_i ij) a
        | otherwise -> othersFn (dp ! pred_ij ij) (dp ! pred_i ij) (dp ! pred_j ij) a
        where
          ij0 = fst (bounds mat)
          i0 = fst ij0
          j0 = snd ij0
          pred_i = first pred
          pred_j = second pred
          pred_ij = bimap pred pred

cum2D :: Matrix -> Matrix
cum2D = scanMatrix id (+) (+) (\predij predi predj val -> predi + predj - predij + val)

printMat :: ((Int, Int), (Int, Int)) -> Matrix -> IO ()
printMat bound mat = do
  let ((r0, c0), (r1, c1)) = bound
  mapM_
    putStrLn
    [ unwords [show (mat ! (i, j)) | j <- [c0 .. c1]]
      | i <- [r0 .. r1]
    ]

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

main :: IO ()
main = do
  (h, w, n) <- ints3
  qs <- replicateM n ints4
  printMat ((1, 1), (h, w)) $ cum2D $ newMatrix ((0, 0), (h + 1, w + 1)) qs