import Control.Monad (replicateM)
import Data.Array (accumArray, elems)

ints :: IO [Int]
ints = map read . words <$> getLine

ints2 = (\[x1, x2] -> (x1, x2)) <$> ints

solve d n lrs =
  -- d+1を除く
  init $
    -- 一次元累積和を計算する
    scanl1 (+) $
      -- ArrayをListに戻す
      elems $
        -- デフォルトは0、重複している場合は(+)を適用しながらArrayを構築する
        accumArray (+) 0 (1, d + 1) $
          -- (index, value)のリストを作る。
          -- 例 (l,r) が (2, 3) -> [(2, 1), (3, -1)]
          concatMap (\(l, r) -> [(l, 1), (r + 1, -1)]) lrs

main = do
  -- 入力
  [d] <- ints
  [n] <- ints
  lrs <- replicateM n ints2
  -- 出力
  mapM_ print (solve d n lrs)