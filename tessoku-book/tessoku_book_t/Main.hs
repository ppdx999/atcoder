module Main (main) where

import Control.Monad (replicateM)
import Data.Array (Array, bounds, listArray, range, (!))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import GHC.Unicode (isSpace)
import Prelude hiding (last)

type Str = Array Int Char

type DP = Array (Int, Int) Int

str :: IO Str
str =
  let toStr s = listArray (1, length s) s
   in BS.getLine <&> toStr . BS.unpack . BS.dropWhile isSpace

last :: Array (Int, Int) Int -> Int
last arr = arr ! snd (bounds arr)

len :: Array Int e -> Int
len arr = let (start, end) = bounds arr in end - start + 1

maxs :: [Int] -> Int
maxs = foldl max 0

solve :: Str -> Str -> DP
solve s t = dp
  where
    bnds = ((0, 0), (len s, len t))
    dp = listArray bnds $ map step (range bnds)
    -- 漸化式
    step (0, 0) = 0
    step (0, j) = 0
    step (i, 0) = 0
    step (i, j)
      | s ! i == t ! j = maxs [left, up, diag + 1]
      | otherwise = max left up
      where
        left = dp ! (i - 1, j) -- 左のマス
        up = dp ! (i, j - 1) -- 上のマス
        diag = dp ! (i - 1, j - 1) -- 左上のマス

main :: IO ()
main = do
  [s, t] <- replicateM 2 str
  let dp = solve s t
   in print $ last dp