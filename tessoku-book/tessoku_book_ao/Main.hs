import Data.List (group)

-- | 連続する同じ要素の個数を数える
-- 例 repeatCount ""aaabbcaaa"" == [3,2,1,3]
--    repeatCount [1,1,1,2,2,3,3,3,] == [3,2,4]
repeatCount as = map length (group as)

yn b = if b then "Yes" else "No"

main :: IO ()
main = do
  -- 入力
  n :: Int <- readLn
  goal <- getLine
  -- 出力
  putStrLn . yn $ any (>= 3) $ repeatCount goal