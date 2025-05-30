main = do
  [n, k] <- map read . words <$> getLine
  let red = [1 .. n]
      blue = [1 .. n]
      possibleAnswers = [k - x - y | x <- red, y <- blue]
      countUpAnswer = length . filter (\x -> 1 <= x && x <= n)
  print $ countUpAnswer possibleAnswers