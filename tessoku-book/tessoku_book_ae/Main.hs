main :: IO ()
main = do
  n <- readLn
  print $ (n `div` 3) + (n `div` 5) - (n `div` 15)