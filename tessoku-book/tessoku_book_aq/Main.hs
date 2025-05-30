import Control.Monad (replicateM)

-- (Int, Char)を入力から読み取る
readPerson :: IO (Int, Char)
readPerson = do
  [a, [b]] <- words <$> getLine
  return (read a, b)

solve n l people =
  -- 最後にトンネルを出る人を見つける
  maximum $ map exitSeconds people
  where
    -- トンネルから出るまでにかかる時間
    exitSeconds (distance, 'W') = distance
    exitSeconds (distance, 'E') = l - distance
    exitSeconds _ = error "unrachable"

main = do
  -- 入力
  [n, l] <- map read . words <$> getLine
  people <- replicateM n readPerson
  -- 出力
  print $ solve n l people