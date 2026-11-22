module Main (main) where

import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.List (foldl', unfoldr)
import Data.Set qualified as Set
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints1 = ints <&> \[x1] -> (x1)

ints2 = ints <&> \[x1, x2] -> (x1, x2)

ints3 = ints <&> \[x1, x2, x3] -> (x1, x2, x3)

ints4 = ints <&> \[x1, x2, x3, x4] -> (x1, x2, x3, x4)

type Weight = Int

type Vertex = Int

type Graph = IM.IntMap (IM.IntMap Weight)

emptyGraph n = IM.fromDistinctAscList [(i, IM.empty) | i <- [1 .. n]]

insertEdge :: Graph -> (Vertex, Vertex, Weight) -> Graph
insertEdge g (u, v, w) = insert u v w $ insert v u w g
  where
    insert u v w = IM.insertWith IM.union v (IM.singleton u w)

type PQ = Set.Set (Weight, Vertex)

type Dist = IM.IntMap Weight

djkstra :: Graph -> Vertex -> Dist
djkstra g start = go pq_0 dist_0
  where
    pq_0 = Set.singleton (0, start)
    dist_0 = IM.empty
    go pq dist
      | Set.null pq = dist
      | otherwise =
          -- pqから重さが最小の頂点とその頂点までの合計コストを取り出す。
          let ((d, u), pq') = Set.deleteFindMin pq
           in -- すでに最短経路が追加済みかを確認
              case IM.lookup u dist of
                -- 追加されていれば何もしない
                Just _ -> go pq' dist
                -- 追加されていなければ
                Nothing ->
                  -- 頂点uに到達するコストはdなのでdistに追加する
                  let dist' = IM.insert u d dist
                      -- 頂点uから伸びる辺の一覧
                      nbrs = g IM.! u
                      -- このあとすぐに使う畳み込みの１ステップ関数
                      -- 頂点uまでの到達コストに自分自身の辺のコスト(w)
                      -- を追加してPQに追加する(=頂点vまでの総到達コストを追加する)
                      step q v w = Set.insert (d + w, v) q
                      -- 頂点uの隣接点をpqに追加する
                      pq'' = IM.foldlWithKey' step pq' nbrs
                   in -- 次の処理へ
                      go pq'' dist'

fillUnreachable n dist = dist `IM.union` IM.fromDistinctAscList [(v, -1) | v <- [1 .. n]]

main :: IO ()
main = do
  -- Input
  (n, m) <- ints2
  inputs <- replicateM m ints3
  let graph = foldl' insertEdge (emptyGraph n) inputs
  -- Solve
  let dist = djkstra graph 1
  -- Answer
  mapM_ (print . snd) $ IM.toAscList $ fillUnreachable n dist
