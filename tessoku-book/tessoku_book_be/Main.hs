import Data.Array (listArray, range, (!))
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
      results = map (stepYFromX dp) qs
  -- 出力
  mapM_ print results

-- 制約より Yj < 2^30 が成立するので29で十分
maxYj = 29

-- DPを構築する(dp[d][i]は穴iから2^d日後にいる穴の番号)
buildDP n aArray = infListArray (0, maxYj) infDpList
  where
    -- dpの無限リスト
    infDpList = iterate diffEquation aArray
    -- 漸化式
    diffEquation dp_prev = arrayWith (1, n) (\i -> dp_prev ! (dp_prev ! i))

-- 穴XからアリがY回動いた後の穴の位置を求める関数
stepYFromX dp (x, y) = foldl step x (bitIndices y)
  where
    -- 現在位置 pos に対して、2^d 日後の位置にジャンプする（ダブリングテーブル dp を参照）
    -- これは dp[d][pos] に相当し、アリが 2^d 回動いたあとの穴を意味する
    step pos d = (dp ! d) ! pos

    -- 与えられた整数 y に対して、2 進数表現でビットが立っている桁のインデックスを返す（下位ビットから数える）
    -- たとえば y = 13 (1101₂) のとき、返り値は [0, 2, 3]
    bitIndices y = filter (testBit y) [0 .. maxYj]

------------------------
-- Helper
------------------------

-- 範囲と無限リストを指定して、Arrayを生成する
infListArray bnds@(lo, hi) infList = listArray bnds $ take (hi - lo + 1) infList

-- 範囲と関数を指定して、Arrayを生成する
arrayWith bnds f = listArray bnds [f i | i <- range bnds]

------------------------
-- In/Out
------------------------

readInts :: BS.ByteString -> [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

toPairs :: [Int] -> [(Int, Int)]
toPairs [] = []
toPairs (x : y : rest) = (x, y) : toPairs rest
toPairs _ = error "Odd number of query parameters"