import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (foldl', unfoldr)
import GHC.Unicode (isSpace)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints :: IO [Int]
ints = map read . words <$> getLine

-- | 配列をトリプルに変換
t3 (x1 : x2 : x3 : xs) = (x1, x2, x3)

-- 区間(l, r)のリストの要素にfを適用したリストを返す
mapWithin (l, r) f xs =
  let (pre, rest) = splitAt (l - 1) xs
      (mid, post) = splitAt (r - l + 1) rest
   in pre ++ map f mid ++ post

-- 解を求める
solve d conditions = sum $ foldl' step (replicate d 24) conditions
  where
    step xs (l, r, h) = mapWithin (l, r) (min h) xs

main = do
  -- 入力
  [d, n] <- ints
  conditions <- replicateM n ints <&> map t3
  -- 出力
  print $ solve d conditions
