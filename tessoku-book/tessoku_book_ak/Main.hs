import Control.Monad (replicateM)
import Data.ByteString.Char8 (dropWhile, getLine, readInt)
import Data.List (unfoldr)
import GHC.Unicode (isSpace)
import Prelude hiding (dropWhile, getLine)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints :: IO [Int]
ints = unfoldr (readInt . dropWhile isSpace) <$> getLine

main :: IO ()
main = do
  -- 入力
  [n, m, b] <- ints
  [as, cs] <- replicateM 2 ints
  -- 計算
  let ans = sum as * m + sum cs * n + b * m * n
  -- 出力
  print ans