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

main :: IO ()
main = do
  (n : k : _) <- ints
  let red = [1 .. n]
  let blue = [1 .. n]
  print $ length $ filter (>= 1) $ filter (<= n) [k - x - y | x <- red, y <- blue]