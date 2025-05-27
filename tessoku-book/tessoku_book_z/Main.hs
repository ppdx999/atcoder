{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints1 :: IO Int
ints1 =
  ints >>= \case
    [x1] -> return x1
    _ -> error "ints1: wrong number of integers"

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Int -> Bool
isPrime n =
  all (\p -> n `mod` p /= 0) $
    takeWhile (\p -> p * p <= n) primes

main :: IO ()
main = do
  [n] <- ints
  xs <- replicateM n ints1
  mapM_ (printYn . isPrime) xs