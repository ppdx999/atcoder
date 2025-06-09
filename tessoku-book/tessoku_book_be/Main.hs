import Data.Array (listArray, (!))
import Data.Bits (Bits (testBit))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (unfoldr)
import GHC.Unicode (isSpace)

main = do
  -- 入力
  input <- BS.getContents
  let (n : q : rest1) = readInts input
      (as, rest2) = splitAt n rest1
      qs = toPairs $ take (2 * q) rest2
      aArray = listArray (1, n) as
  -- 計算
  let dp = buildDP n aArray
      results = map (posInYDaysFromX dp) qs
  -- 出力
  mapM_ print results

-- 制約より Yj < 2^30 が成立するので29で十分
maxYj = 29

-- DPを構築する(dp[d][i]は穴iから2^d日後にいる穴の番号)
buildDP n aArray = dp
  where
    -- DPを構築する
    dp = listArray (0, maxYj) dpList
    -- dp[0] から dp[maxYj] のリスト
    dpList = reverse $ foldl step [] [0 .. maxYj]
    -- ステップ関数
    step acm 0 = aArray : acm
    step acm@(prev : rest) d = diffEquation prev : acm
    step _ _ = error "buildDP: unreachable"
    -- 漸化式
    diffEquation dp_prev = listArray (1, n) [dp_prev ! (dp_prev ! i) | i <- [1 .. n]]

-- 穴XからY日後の穴の位置を求める
posInYDaysFromX dp (x, y) = go 0 x
  where
    go d pos
      | d > maxYj = pos
      | testBit y d = go (d + 1) ((dp ! d) ! pos)
      | otherwise = go (d + 1) pos

------------------------
-- In/Out
------------------------

readInts :: BS.ByteString -> [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

toPairs :: [Int] -> [(Int, Int)]
toPairs [] = []
toPairs (x : y : rest) = (x, y) : toPairs rest
toPairs _ = error "Odd number of query parameters"