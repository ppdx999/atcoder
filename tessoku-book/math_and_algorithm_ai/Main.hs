{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array.Unboxed (Array, listArray, (!))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List qualified as L
import Data.Traversable (for)
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

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

csum1D n = listArray (0, n) . L.scanl' (+) 0

main :: IO ()
main = do
  (n, q) <- ints2
  as <- ints <&> csum1D n
  qs <- replicateM q ints2
  mapM_ (print . process as) qs

process :: Array Int Int -> (Int, Int) -> Int
process as (l, r) = (as ! r) - (as ! pred l)