module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

within :: Int -> Int -> [Int] -> [Int]
within l r = filter (\x -> l <= x && x <= r)

main :: IO ()
main = do
  (n : k : _) <- ints
  let red = [1 .. n]
  let blue = [1 .. n]
  print $ length $ within 1 n [k - x - y | x <- red, y <- blue]