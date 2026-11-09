module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (foldl', unfoldr, intercalate)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import GHC.Unicode (isSpace)
import Control.Monad (replicateM)
import Data.Foldable (for_)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 = ints <&> \[x1,x2] -> (x1,x2)

data Graph = Graph { ungraph :: IM.IntMap IS.IntSet }
  deriving (Show, Eq)

emptyGraph :: Int -> Graph
emptyGraph n = Graph $ IM.fromDistinctAscList [(i, IS.empty) | i <- [1..n]]

insert :: Graph -> (Int, Int) -> Graph
insert (Graph g) (u, v) =
  Graph $ IM.insertWith IS.union u (IS.singleton v)
        $ IM.insertWith IS.union v (IS.singleton u) g

showSet :: Show a => [a] -> String
showSet xs = "{" ++ intercalate ", " (map show xs) ++ "}"

showEntity :: (Int, IS.IntSet) -> String
showEntity (vertex, edgeSet) = show vertex ++ ": " ++ showSet (IS.toAscList edgeSet)

printGraph :: Graph -> IO ()
printGraph (Graph g) = for_ (IM.toAscList g) (putStrLn . showEntity)

main :: IO ()
main = do
  -- Input
  [n,m] <- ints
  edges <- replicateM m ints2
  -- Answer
  printGraph $ foldl' insert (emptyGraph n) edges

