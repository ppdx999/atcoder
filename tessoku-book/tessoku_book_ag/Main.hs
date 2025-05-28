import Data.Bits (Bits (xor))

main = do
  _ <- getLine
  as :: [Int] <- map read . words <$> getLine
  putStrLn $ if (/= 0) $ foldl1 xor as then "First" else "Second"