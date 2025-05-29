import Data.ByteString.Char8 qualified as BS
import Data.List (group, sort, unfoldr)
import GHC.Unicode (isSpace)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | N個から3個選ぶときの組み合わせの数
nC3 n = n * (n - 1) * (n - 2) `div` 6

-- | 連続する同じ要素の個数を数える
-- 例 repeatCount ""aaabbcaaa"" == [3,2,1,3]
--    repeatCount [1,1,1,2,2,3,3,3,] == [3,2,4]
repeatCount as = map length (group as)

main :: IO ()
main = do
  -- 入力
  n :: Int <- readLn
  as <- ints
  -- 出力
  print $ sum $ map nC3 $ repeatCount $ sort as