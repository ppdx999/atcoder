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

main :: IO ()
main = do
  (h, w) <- ints2
  mat <- getMat h w <&> cum2D . to1Index
  q <- ints1
  qs <- replicateM q ints4
  mapM_ (print . calcSum mat) qs

calcSum :: Matrix -> (Int, Int, Int, Int) -> Int
calcSum mat (a, b, c, d) =
  (mat ! (a - 1, b - 1)) + (mat ! (c, d)) - (mat ! (a - 1, d)) - (mat ! (c, b - 1))