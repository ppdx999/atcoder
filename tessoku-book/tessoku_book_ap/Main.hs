import Control.Monad (liftM2, replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (unfoldr)
import GHC.Unicode (isSpace)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints :: IO [Int]
ints = map read . words <$> getLine

ints2 = ints <&> \[x1, x2] -> (x1, x2)

-- | 範囲内かどうかを判定する
within x (lo, hi) = lo <= x && x <= hi

-- | [1.. 100]のリストのデカルト積
list100x100 = liftM2 (,) [1 .. 100] [1 .. 100]

solve n k students =
  -- 条件を満たす生徒数の最大値を見つける
  maximum $ map count list100x100
  where
    -- 体力がa以上a+k以下、気力がb以上b+k以下の生徒数を数える
    count (a, b) = length $ filter check students
      where
        -- 体力、気力の差分がK以下であるか判定
        check (ai, bi) = ai `within` (a, a + k) && bi `within` (b, b + k)

main :: IO ()
main = do
  -- 入力
  [n, k] <- ints
  students <- replicateM n ints2
  -- 出力
  print $ solve n k students