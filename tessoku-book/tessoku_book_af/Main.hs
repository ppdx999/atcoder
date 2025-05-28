import Data.Array (Array, listArray, (!))

buildDP n a b = dp
  where
    -- 畳み込み
    dp = listArray (0, n) (step <$> [0 .. n])
    -- 漸化式
    step i
      | i >= a && not (dp ! (i - a)) = True
      | i >= b && not (dp ! (i - b)) = True
      | otherwise = False

main = do
  -- 入力
  [n, a, b] <- map read . words <$> getLine
  -- 計算
  let dp = buildDP n a b
  -- 出力
  putStrLn $ if dp ! n then "First" else "Second"