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

csum :: [Int] -> [Int]
csum = L.scanl' (+) 0

toArray :: [Int] -> Array Int Int
toArray as = listArray (0, length as - 1) as

main :: IO ()
main = do
  (n, q) <- ints2
  as <- ints <&> (toArray . csum)
  qs <- replicateM q ints2
  let countGuests (l, r) = (as ! r) - (as ! pred l)
   in mapM_ (print . countGuests) qs