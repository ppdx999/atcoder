{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array.Unboxed (IArray (bounds), Ix (range), UArray, accumArray, assocs, elems, listArray, (!))
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints1 :: IO Int
ints1 =
  ints >>= \case
    [x] -> return x
    _ -> error "ints1: wrong number of integers"

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

newArr :: (Int, Int) -> [(Int, Int)] -> UArray Int Int
newArr bound lrs =
  accumArray (+) 0 bound $
    concatMap parse lrs
  where
    parse :: (Int, Int) -> [(Int, Int)]
    parse (l, r) = [(l, 1), (r + 1, -1)]

cum1D :: UArray Int Int -> UArray Int Int
cum1D arr = listArray (bounds arr) (scanl1 (+) (elems arr))

main :: IO ()
main = do
  d <- ints1
  n <- ints1
  lrs <- replicateM n ints2
  let acm = cum1D $ newArr (0, d + 1) lrs

  mapM_ print [acm ! i | i <- [1 .. d]]