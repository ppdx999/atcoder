import Control.Monad (foldM_, replicateM)
import Data.Set qualified as S

main = do
  n <- readLn
  qs <- replicateM n readQ
  foldM_ execQ empty qs

---------------
-- 優先度付きキュー

newtype PriorityQueue a = PriorityQueue (S.Set a)

-- | キューに要素を追加
push :: (Ord a) => PriorityQueue a -> a -> PriorityQueue a
push (PriorityQueue s) x = PriorityQueue (S.insert x s)

-- | キューの先頭を取得
top :: PriorityQueue a -> a
top (PriorityQueue s) = S.findMin s

-- | キューの先頭を削除
pop :: PriorityQueue a -> PriorityQueue a
pop (PriorityQueue s) = PriorityQueue (S.deleteMin s)

-- | 空のキューを作成する
empty :: PriorityQueue a
empty = PriorityQueue S.empty

---------------

data Query = Push Int | Top | Pop

readQ = do
  (op : rest) <- words <$> getLine
  return $ case op of
    "1" -> Push (read $ head rest)
    "2" -> Top
    "3" -> Pop

execQ pq (Push x) = return $ push pq x
execQ pq Top = do
  print $ top pq
  return pq
execQ pq Pop = return $ pop pq