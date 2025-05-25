module Main (main) where

import Control.Monad (replicateM)
import Data.Array ((!))
import Data.Array qualified as A
import Data.Bits (shiftL, (.|.))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)

-------------------------------------------------------
-- Helper
-------------------------------------------------------
ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | 上位ビットから並んだ 0/1 のリストを Int に変換
-- 例: [1,0,1] => 5   (5は二進数で101を意味する)
bitsToInt :: [Int] -> Int
bitsToInt = L.foldl' step 0
  where
    step acc b
      | b == 0 || b == 1 = (acc `shiftL` 1) .|. b
      | otherwise = error "bitsToInt: invalid bit"

-- | n桁のビット列の最大値
-- 例: fullBits 3 => 7   (7は二進数で111を意味する)
fullBits :: Int -> Int
fullBits n = (1 `shiftL` n) - 1

-- | Intの最大値
maxInt :: Int
maxInt = 10 ^ 9

-------------------------------------------------------
-- BitSet
-------------------------------------------------------
newtype BitSet = BitSet {unBitSet :: Int} deriving (Eq, Ord, A.Ix)

-- 空集合
emptySet :: BitSet
emptySet = BitSet 0

-- 和集合演算
(\/) :: BitSet -> BitSet -> BitSet
(\/) (BitSet a) (BitSet b) = BitSet (a .|. b)

-------------------------------------------------------
-- Main
-------------------------------------------------------
solve :: Int -> [BitSet] -> A.Array BitSet Int
solve n = L.foldl step dp_0 -- 畳み込み
  where
    -- 初期値
    dp_0 = A.listArray (BitSet 0, BitSet (fullBits n)) (0 : replicate (fullBits n) maxInt)

    -- dp[i]とT[i]からdp[i+1]を計算する
    step :: A.Array BitSet Int -> BitSet -> A.Array BitSet Int
    step dp_i t_i =
      let bnds = A.bounds dp_i
       in -- 漸化式
          A.accumArray min maxInt bnds $
            [(s, dp_i ! s) | s <- A.range bnds]
              ++ [(s \/ t_i, (dp_i ! s) + 1) | s <- A.range bnds]

printResult :: Int -> IO ()
printResult n
  | n == maxInt = print (-1)
  | otherwise = print n

main :: IO ()
main = do
  [n, m] <- ints
  -- \| BitSet . bitsToInt . reverseは入力を[BitSet]に変換
  -- 例: [1,0,1] → BitSet 5
  coupons <- replicateM m ints <&> map (BitSet . bitsToInt . reverse)
  let dp = solve n coupons
      last = dp ! BitSet (fullBits n)
  printResult last