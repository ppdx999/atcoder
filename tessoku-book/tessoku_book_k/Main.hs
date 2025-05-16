{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

-- ///////////////////////////////////////////////////////////////////////
-- TEMPLATE
-- ///////////////////////////////////////////////////////////////////////

----------------
-- In / Out
----------------

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

----------------
-- Tree
----------------

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Show)

type Index = Int

type Value = Int

search :: Value -> Tree (Value, Index) -> Maybe Int
search _ Nil = Nothing
search x (Node (val, idx) l r) = case compare x val of
  EQ -> Just idx
  LT -> search x l
  GT -> search x r

buildTree :: [Value] -> Tree (Value, Index)
buildTree xs = fst (go (length xs) (zip xs [1 ..]))
  where
    go 0 xs = (Nil, xs)
    go n xs =
      let (leftSize, rightSize) = (n `div` 2, n - n `div` 2 - 1)
          (leftTree, x1 : xs1) = go leftSize xs
          (rightTree, xs2) = go rightSize xs1
       in (Node x1 leftTree rightTree, xs2)

-- ///////////////////////////////////////////////////////////////////////
-- Main
-- ///////////////////////////////////////////////////////////////////////

maybeToInt :: Maybe Int -> Int
maybeToInt Nothing = 0
maybeToInt (Just x) = x

main :: IO ()
main = do
  (_, x) <- ints2
  as <- ints
  print . maybeToInt $ search x (buildTree as)