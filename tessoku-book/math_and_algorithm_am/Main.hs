module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List (unfoldr, foldl')
import Data.Functor ((<&>))
import GHC.Unicode (isSpace)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Control.Monad.State (modify', State, MonadState (get), execState)
import Control.Monad (replicateM)


ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 = ints <&> \[x1,x2] -> (x1,x2)

data Graph = Graph { ungraph :: IM.IntMap IS.IntSet }
type Vertex = Int
type Edge = (Vertex, Vertex)
type Visited = IS.IntSet 

emptyGraph :: Int -> Graph
emptyGraph n = Graph $ IM.fromDistinctAscList [(i, IS.empty) | i <- [1..n]]

insert :: Graph -> Edge -> Graph
insert (Graph g) (u, v) = Graph $ addEdge u v $ addEdge v u g
  where
    addEdge u v = IM.insertWith IS.union u (IS.singleton v)

dfs :: Graph -> Vertex -> State Visited ()
dfs (Graph g) u = do
  visited <- get
  if u `IS.member` visited
    then pure ()
    else do
      modify' (IS.insert u)
      mapM_ (dfs (Graph g)) (IS.toAscList (g IM.! u))

connected  :: Graph -> Int -> Bool
connected g n = IS.size (execState (dfs g 1) IS.empty) == n

printAns True = putStrLn "The graph is connected."
printAns False = putStrLn "The graph is not connected."

main :: IO ()
main = do
  -- Input
  (n,m) <- ints2
  edges <- replicateM m ints2
  let graph = foldl' insert (emptyGraph n) edges
  -- Answer
  printAns $ connected graph n
