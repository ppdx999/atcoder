{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 (dropWhile, getLine, readInt)
import Data.List (unfoldr)
import GHC.Unicode (isSpace)
import Prelude hiding (dropWhile, getLine)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints :: IO [Int]
ints = unfoldr (readInt . dropWhile isSpace) <$> getLine

ints1 :: IO Int
ints1 =
  ints >>= \case
    [x1] -> return x1
    _ -> error "ints1: wrong number of integers"

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

ints3 :: IO (Int, Int, Int)
ints3 =
  ints >>= \case
    [x1, x2, x3] -> return (x1, x2, x3)
    _ -> error "ints3: wrong number of integers"

ints4 :: IO (Int, Int, Int, Int)
ints4 =
  ints >>= \case
    [x1, x2, x3, x4] -> return (x1, x2, x3, x4)
    _ -> error "ints4: wrong number of integers"

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

-- ///////////////////////////////////////////////////////////////////////
-- Main
-- ///////////////////////////////////////////////////////////////////////

main :: IO ()
main = do
  -- 入力
  -- 計算
  -- 出力
  print 0