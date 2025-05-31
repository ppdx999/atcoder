import Control.Monad (foldM_, replicateM)

main = do
  n <- readLn
  qs <- replicateM n readQ
  foldM_ execQ empty qs

------------------------------------
newtype Queue a = Queue [a]

push (Queue qs) x = Queue (qs ++ [x])

front (Queue (q : qs)) = q

pop (Queue (q : qs)) = Queue qs

empty = Queue []

------------------------------------
data Query = Push String | Print | Pop

readQ = do
  (op : rest) <- words <$> getLine
  return $ case op of
    "1" -> Push (head rest)
    "2" -> Print
    "3" -> Pop

execQ queue (Push x) = return $ push queue x
execQ queue Print = do
  putStrLn $ front queue
  return queue
execQ queue Pop = return $ pop queue
