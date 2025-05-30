import Control.Monad (replicateM)
import Data.Array.Unboxed (Array, listArray, (!))
import Data.Functor ((<&>))

-- | 入力から数値のリストを受け取る
ints :: IO [Int]
ints = map read . words <$> getLine

ints2 = ints <&> \[x1, x2] -> (x1, x2)

-- | 一次元累積和を計算する
cum1D :: [Int] -> [Int]
cum1D = scanl1 (+)

toArray :: [Int] -> Array Int Int
toArray as = listArray (0, length as) (0 : as)

solve n q as qs = nGuests
  where
    -- ゲストの数 :: [Int]
    nGuests = map count qs

    -- l~r日の来場者を数える
    count (l, r) = (arr ! r) - (arr ! pred l)

    -- Ai (1~N)の累積和
    arr = toArray $ cum1D as

main = do
  -- 入力
  (n, q) <- ints2
  as <- ints
  qs <- replicateM q ints2
  -- 出力
  mapM_ print $ solve n q as qs