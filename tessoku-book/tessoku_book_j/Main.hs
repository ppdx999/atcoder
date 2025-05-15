{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)

-- ///////////////////////////////////////////////////////////////////////
-- TEMPLATE
-- ///////////////////////////////////////////////////////////////////////

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

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

sandwich :: Int -> [Int] -> [Int]
sandwich n arr = [n] ++ arr ++ [n]

scanArray :: ((Int -> Int -> Int) -> [Int] -> [Int]) -> (Int -> Int -> Int) -> [Int] -> UArray Int Int
scanArray scanner f list =
  listArray (0, length list - 1) (scanner f list)

findMax :: UArray Int Int -> UArray Int Int -> (Int, Int) -> Int
findMax lmax rmax (l, r) = max (lmax ! (l - 1)) (rmax ! (r + 1))

main :: IO ()
main = do
  n <- ints1
  as <- ints <&> sandwich 0
  q <- ints1
  qs <- replicateM q ints2

  let lmax = scanArray scanl1 max as
      rmax = scanArray scanr1 max as
   in mapM_ (print . findMax lmax rmax) qs