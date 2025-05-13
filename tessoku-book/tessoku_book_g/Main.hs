{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints1 :: IO Int
ints1 =
  ints >>= \case
    [x] -> return x
    _ -> error "ints1: wrong number of integers"

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

main :: IO ()
main = do
  d <- ints1
  n <- ints1
  lrs <- replicateM n ints2

  let diffMap = foldl insertDiff Map.empty lrs
      diffs = [Map.findWithDefault 0 i diffMap | i <- [1 .. d + 1]]
      res = take d $ tail $ scanl (+) 0 diffs
  mapM_ print res

insertDiff :: Map.Map Int Int -> (Int, Int) -> Map.Map Int Int
insertDiff m (l, r) =
  Map.insertWith (+) l 1 $ Map.insertWith (+) (r + 1) (-1) m
