yn b = if b then "Yes" else "No"

solve n k
  | k < (2 * n - 2) = False
  | even k = True
  | odd k = False
  | otherwise = error "unreachable"

main = do
  -- 入力
  [n, k] <- map read . words <$> getLine
  -- 出力
  putStrLn . yn $ solve n k