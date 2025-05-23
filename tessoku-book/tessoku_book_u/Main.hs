{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array (Array, accumArray, bounds, listArray, range, (!))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (unfoldr)
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

maxs :: [Int] -> Int
maxs = foldl max 0

type Blocks = Array Int (Int, Int)

type DP = Array (Int, Int) Int

buildDP :: Int -> Blocks -> DP
buildDP n blocks = dp
  where
    -- Utils
    bnds = ((1, 1), (n, n))
    p i = fst $ blocks ! i
    a i = snd $ blocks ! i
    -- dpを計算
    dp = listArray bnds $ map step (range bnds)
    -- 漸化式
    step (l, r)
      | l == 1 && r == n = 0 -- 両端
      | l == 1 = rightScore -- 左端
      | r == n = leftScore -- 右端
      | otherwise = max leftScore rightScore
      where
        score i
          | l <= p i && p i <= r = a i
          | otherwise = 0
        leftScore = score (l - 1) + dp ! (l - 1, r)
        rightScore = score (r + 1) + dp ! (l, r + 1)

main :: IO ()
main = do
  [n] <- ints
  blocks <- replicateM n ints2 <&> listArray (1, n)
  let dp = buildDP n blocks
      possibleAnswers = [dp ! (i, i) | i <- [1 .. n]]
   in print $ maxs possibleAnswers