module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import Distribution.Utils.Generic (safeHead)
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
  (n : x : _) <- ints
  arr <- ints

  printYn $ x `elem` arr