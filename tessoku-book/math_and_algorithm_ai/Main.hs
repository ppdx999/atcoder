{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array.Unboxed (Array, listArray, (!))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

csum1D :: [Int] -> Array Int Int
csum1D as = listArray (0, length as) $ L.scanl' (+) 0 as

main :: IO ()
main = do
  (n, q) <- ints2
  as <- ints <&> csum1D
  qs <- replicateM q ints2
  mapM_ (print . countGuests as) qs

countGuests :: Array Int Int -> (Int, Int) -> Int
countGuests as (l, r) = (as ! r) - (as ! pred l)