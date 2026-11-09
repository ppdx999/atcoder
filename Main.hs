{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (unfoldr)
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints1 = ints <&> \[x1] -> (x1)
ints2 = ints <&> \[x1,x2] -> (x1,x2)
ints3 = ints <&> \[x1,x2,x3] -> (x1,x2,x3)
ints4 = ints <&> \[x1,x2,x3,x4] -> (x1,x2,x3,x4)

main :: IO ()
main = do
  -- Input
  -- Answer
  print 0
