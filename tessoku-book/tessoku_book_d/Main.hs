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

toBinary :: Int -> String
toBinary 0 = ""
toBinary n = toBinary (n `div` 2) ++ show (n `mod` 2)

main :: IO ()
main = do
  (n : _) <- ints
  let bin = toBinary n
  putStrLn (replicate (10 - length bin) '0' ++ bin)