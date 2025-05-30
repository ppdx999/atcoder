ints :: IO [Int]
ints = map read . words <$> getLine

toBinary 0 = ""
toBinary n = toBinary (n `div` 2) ++ show (n `mod` 2)

padding10 str = replicate (10 - length str) '0' ++ str

solve = padding10 . toBinary

main = do
  -- 入力
  (n : _) <- ints
  -- 出力
  putStrLn $ solve n