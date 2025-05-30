ints :: IO [Int]
ints = map read . words <$> getLine

yn True = "Yes"
yn False = "No"

main = do
  -- 入力
  [n, x] <- ints
  arr <- ints
  -- 出力
  putStrLn . yn $ x `elem` arr
