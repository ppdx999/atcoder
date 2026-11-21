module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List (unfoldr, foldl')
import Data.Functor ((<&>))
import GHC.Unicode (isSpace)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Control.Monad.State (modify', State, MonadState (get), execState)
import Control.Monad (replicateM, forM_)


ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 = ints <&> \[x1,x2] -> (x1,x2)

type Graph = IM.IntMap IS.IntSet
type Vertex = Int
type Edge = (Vertex, Vertex)
type Distance = IM.IntMap Int

emptyGraph :: Int -> Graph
emptyGraph n = IM.fromDistinctAscList [(i, IS.empty) | i <- [1..n]]

insert :: Graph -> Edge -> Graph
insert g (u, v) = addEdge u v $ addEdge v u g
  where
    addEdge u v = IM.insertWith IS.union u (IS.singleton v)

distance :: Graph -> Vertex -> Distance
distance g start = go frontier_0 visited_0 dist_0 max_d_0
  where
    frontier_0 = IS.singleton start
    visited_0 = IS.singleton start
    dist_0 = IM.singleton start 0
    max_d_0 = 0
    go frontier visited dist max_d
      | IS.null frontier = dist
      | otherwise =
          let
              nbrs = IS.unions [g IM.! u | u <- IS.toAscList frontier]
              next = nbrs `IS.difference` visited
              visited' = visited `IS.union` next
              dist' = dist `IM.union` IM.fromSet (const (max_d+1)) next
          in go next visited' dist' (max_d+1)

fillBlank :: Int -> Distance -> Distance
fillBlank n d =
  let c = IS.fromList [ 1.. n] `IS.difference` IM.keysSet d
  in d `IM.union` IM.fromSet (const (-1)) c

main :: IO ()
main = do
  -- Input
  (n,m) <- ints2
  edges <- replicateM m ints2
  let graph = foldl' insert (emptyGraph n) edges
  -- Solve
  let answers =  snd <$> (IM.toAscList $ fillBlank n $ distance graph 1)
  -- Answer
  mapM_ print answers
