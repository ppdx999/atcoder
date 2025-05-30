main = do
  -- 入力
  [n] <- map read . words <$> getLine
  -- 出力
  print (n * n)