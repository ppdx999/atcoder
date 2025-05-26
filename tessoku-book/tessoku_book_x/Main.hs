module Main (main) where

import Control.Monad.ST (ST, runST)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (foldl'), forM_, for_)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import GHC.Unicode (isSpace)

------------------------------------------------------------
-- Utils
------------------------------------------------------------

-- | 複数のIntの入力を受け取る
ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | 二分探索
binSearch :: (Int -> Bool) -> (Int, Int) -> Int
binSearch check (ng, ok) = go ng ok
  where
    go ng ok
      | ok - ng > 1 = go ng' ok'
      | otherwise = ok
      where
        mid = (ng + ok) `div` 2
        (ng', ok') = if check mid then (ng, mid) else (mid, ok)

-- | Intの最大値
inf :: Int
inf = 10 ^ 9

------------------------------------------------------------
-- Array
------------------------------------------------------------

type Index = Int

data Array e = Array
  { bound :: (Index, Index),
    m :: M.Map Index e
  }

-- | 計算量O(logN)でArrayの要素iにアクセスする(範囲外のアクセスはエラー)
(!) :: Array e -> Index -> e
(!) arr i = fromMaybe (error "Array (!) out of bounds") (M.lookup i (m arr))

-- | 計算量O(logN)でArrayの要素を更新する
(<--) :: Array e -> (Index, e) -> Array e
(<--) arr (i, a) = Array (bound arr) (M.insert i a (m arr))

-- | 計算量O(logN)でArrayの中でxより大きい最初の要素のindexを返す
lowerBound :: (Ord e) => Array e -> e -> Int
lowerBound arr x = binSearch ((>= x) . (arr !)) (bound arr)

-- | 要素がすべて同じArray eを返す
constArray :: (Index, Index) -> e -> Array e
constArray bnds@(lo, hi) a = Array bnds (M.fromList [(k, a) | k <- [lo .. hi]])

------------------------------------------------------------
-- Main
------------------------------------------------------------
solve :: Int -> [Int] -> Int
solve n as = final
  where
    -- 畳み込み
    (final, _) = foldl step init as

    -- 初期状態
    init = (0, constArray (0, 100009) inf)

    -- 漸化式
    step :: (Int, Array Int) -> Int -> (Int, Array Int)
    step (maxLen, arrL) a =
      let pos = lowerBound arrL a
       in (max maxLen pos, arrL <-- (pos, a))

main :: IO ()
main = do
  [n] <- ints
  as <- ints
  print $ solve n as