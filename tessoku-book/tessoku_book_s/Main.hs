{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array.Unboxed (IArray, Ix, UArray, array, bounds, listArray, range, (!))
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)
import Prelude hiding (last)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

last :: UArray Int Int -> Int
last arr = arr ! snd (bounds arr)

scanlByIndex :: (Ix i, IArray UArray e) => (i -> e) -> UArray i e -> UArray i e
scanlByIndex f arr = listArray (bounds arr) [f i | i <- range (bounds arr)]

step :: (Int, Int) -> UArray Int Int -> UArray Int Int
step (w_i, v_i) dp_i = scanlByIndex diffEquation dp_i
  where
    -- 漸化式
    diffEquation w
      | w >= w_i = max (dp_i ! (w - w_i) + v_i) (dp_i ! w)
      | otherwise = dp_i ! w

main :: IO ()
main = do
  [n, w] <- ints
  items <- replicateM n ints2
  let dp_0 = array (0, w) [(i, 0) | i <- [0 .. w]]
      dp = foldr step dp_0 items
  print $ last dp