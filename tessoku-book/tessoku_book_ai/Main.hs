import Data.Array (Array, Ix, bounds, listArray, range, (!))
import Data.ByteString.Char8 (dropWhile, getLine, readInt)
import Data.Functor ((<&>))
import Data.List (unfoldr)
import GHC.Unicode (isSpace)
import Prelude hiding (dropWhile, getLine)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints :: IO [Int]
ints = unfoldr (readInt . dropWhile isSpace) <$> getLine

-- | Ixの範囲にビルダー関数を適用してArrayを構築する
buildArr :: (Ix a) => (a, a) -> (a -> e) -> Array a e
buildArr bnds builder = listArray bnds (builder <$> range bnds)

solve :: Int -> Array Int Int -> Int
solve n as = dp ! (1, 1)
  where
    -- 畳み込み
    dp = buildArr ((1, 1), (n, n)) go
    -- 漸化式
    go (i, j)
      | i == n = as ! j
      | even i = min (dp ! (i + 1, j)) (dp ! (i + 1, j + 1))
      | odd i = max (dp ! (i + 1, j)) (dp ! (i + 1, j + 1))
      | otherwise = error "unreachable"

main :: IO ()
main = do
  -- 入力
  [n] <- ints
  as <- ints <&> listArray (1, n)
  -- 出力
  print $ solve n as