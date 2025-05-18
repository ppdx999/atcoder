{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (sort)
import Data.List qualified as L
import GHC.Unicode (isSpace)

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

----------------
-- Tree
----------------

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Show)

type Index = Int

type Value = Int

search :: Tree (Value, Index) -> Value -> Int
search Nil _ = error "search: not found"
search (Node (val, idx) l r) x = case compare x val of
  EQ -> idx
  LT -> search l x
  GT -> search r x

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

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x : y : xs)
  | x == y = uniq (y : xs)
  | otherwise = x : uniq (y : xs)

printInts :: [Int] -> IO ()
printInts = putStrLn . unwords . map show

main :: IO ()
main = do
  n <- ints1
  as <- ints
  let aTree = buildTree $ uniq $ sort as
      bs = map (search aTree) as
   in printInts bs