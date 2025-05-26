module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import GHC.Unicode (isSpace)

----------------
-- In / Out
----------------

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

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

main :: IO ()
main = do
  [_n, x] <- ints
  as <- ints
  print $ fromMaybe 0 (search x (buildTree as))