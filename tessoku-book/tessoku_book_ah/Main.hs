import Data.Array
import Data.Bits (xor)
import Data.ByteString.Char8 qualified as BS
import Data.List (unfoldr)
import GHC.Unicode (isSpace)
import Prelude hiding (dropWhile, getLine)

-- | 標準入力から空白区切りの整数列を読み込む
ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | 2つの値に対するmex（minimum excluded value）を高速に求める
mex2 :: Int -> Int -> Int
mex2 a b
  | 0 `notElem` [a, b] = 0
  | 1 `notElem` [a, b] = 1
  | otherwise = 2

main :: IO ()
main = do
  [n, x, y] <- ints
  as <- ints
  let maxA = maximum as
      -- Grundy数の配列：grundy ! i は i個の石に対するGrundy数
      grundy :: Array Int Int
      grundy = array (0, maxA) [(i, g i) | i <- [0 .. maxA]]
      -- 一般のmex関数：与えられたリストに含まれない最小の非負整数を返す
      mex xs = head $ filter (`notElem` xs) [0 ..]
      -- Grundy数の遷移関数：mex{g(i - x), g(i - y)}（ただし i >= x または y）
      g i = mex [grundy ! (i - t) | t <- [x, y], i >= t]
      -- 各山に対応するGrundy数のXORを全体でとる
      total = foldl xor 0 $ map (grundy !) as
  -- 出力
  putStrLn $ if total /= 0 then "First" else "Second"
