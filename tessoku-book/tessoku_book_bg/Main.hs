import Data.ByteString.Char8 qualified as BS
import Data.List (unfoldr)
import GHC.Unicode (isSpace)

main = input >>= output . solve

-- クエリの種類（1 pos x または 2 l r）
data Q = Update Int Int | Query Int Int

solve (n, q, qs) = results
  where
    -- 積み上げたクエリ結果を答えの順番に直す
    results = reverse qResults
    -- 各クエリを順番に処理して、状態（セグ木とクエリ結果）を畳みこみ
    (_, qResults) = foldl step initState qs
    -- 初期状態
    initState = (buildTree 1 (n + 1), [])
    -- 状態変化(セグ木の更新)
    step (tree, qResults) (Update pos x) = (update pos x tree, qResults)
    -- 状態変化(合計値の取得)
    step (tree, qResults) (Query l r) = (tree, query l r tree : qResults)

--------------------------------------
-- Segment Tree
--------------------------------------

-- | SegmentTree は区間に関する情報（ここでは最大値）を管理する木構造です。
data SegmentTree
  = -- 葉ノード：1 つの要素（インデックスとその値）を表します。
    Leaf
      { -- | 対応する配列インデックス
        index :: Int,
        -- | この位置に格納されている値
        value :: Int
      }
  | -- 内部ノード：左右の部分木とその区間 [lBound, rBound) に対応する最大値を持ちます。
    Node
      { -- | 左の部分木（[lBound, m) を担当）(m=(lBound+rBound)/2)
        left :: SegmentTree,
        -- | 右の部分木（[m, rBound) を担当）(m=(lBound+rBound)/2)
        right :: SegmentTree,
        -- | このノードが担当する区間の左端（閉区間）
        lBound :: Int,
        -- | このノードが担当する区間の右端（開区間）
        rBound :: Int,
        -- | この区間 [lBound, rBound) における最大値
        val :: Int
      }
  deriving (Show)

-- 木に保持する演算（今回は足し算）
treeOp = (+)

-- 範囲外の探索をしたときに返却する値（今回は0）
ignore = 0 :: Int

-- セグメント木を構築する（[l, r) を担当するノードを作成）
buildTree :: Int -> Int -> SegmentTree
buildTree l r
  | l + 1 == r = Leaf l 0
  | otherwise = Node left right l r (treeOp (getVal left) (getVal right))
  where
    m = (l + r) `div` 2
    left = buildTree l m
    right = buildTree m r

-- ノードから値を取得
getVal :: SegmentTree -> Int
getVal (Leaf _ v) = v
getVal (Node _ _ _ _ v) = v

-- セグメント木の更新（位置iの値をxに更新）
update :: Int -> Int -> SegmentTree -> SegmentTree
update i x (Leaf j _)
  | i == j = Leaf j x
  | otherwise = error "Index mismatch"
update i x (Node l r lb rb _)
  | i < m = let l' = update i x l in Node l' r lb rb (treeOp (getVal l') (getVal r))
  | otherwise = let r' = update i x r in Node l r' lb rb (treeOp (getVal l) (getVal r'))
  where
    m = (lb + rb) `div` 2

-- 区間 [ql, qr) の最大値を取得
query :: Int -> Int -> SegmentTree -> Int
query ql qr (Leaf i v)
  | ql <= i && i < qr = v -- 範囲内なら値をそのまま返す
  | otherwise = ignore -- 範囲外なら無視する
query ql qr (Node l r lb rb mv)
  | qr <= lb || rb <= ql = ignore -- 範囲外なら無視する
  | ql <= lb && rb <= qr = mv -- 完全に範囲内なら値をそのまま返す
  | otherwise = treeOp (query ql qr l) (query ql qr r) -- 部分的に重なる場合はその他のクエリに処理を委譲する

--------------------------------------
-- 入出力処理
--------------------------------------

input = do
  cnt <- BS.getContents
  let (n : q : rest) = readInts cnt
      qs = take q $ parseQ rest
  return (n, q, qs)
  where
    -- 空白区切りで整数列に変換
    readInts = unfoldr (BS.readInt . BS.dropWhile isSpace)

    -- クエリのパース
    parseQ [] = []
    parseQ (1 : pos : x : rest) = Update pos x : parseQ rest
    parseQ (2 : l : r : rest) = Query l r : parseQ rest

output = mapM_ print