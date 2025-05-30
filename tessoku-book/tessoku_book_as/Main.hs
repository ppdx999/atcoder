toInt 'W' = 0
toInt 'R' = 1
toInt 'B' = 2
toInt _ = error "invalid card"

yn b = if b then "Yes" else "No"

main = do
  [_, [c]] <- words <$> getLine
  line <- getLine
  putStrLn . yn $ toInt c == (sum (map toInt line) `mod` 3)