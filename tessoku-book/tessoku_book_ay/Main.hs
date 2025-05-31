import Control.Monad (foldM_, replicateM)

-----------------------------------------
newtype Stack a = Stack [a]

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x : xs)

pop :: Stack a -> Stack a
pop (Stack xs) = Stack (tail xs)

top :: Stack a -> a
top (Stack xs) = head xs

empty :: Stack a
empty = Stack []

-----------------------------------------

data Query = Push String | Print | Pop

readQuery :: IO Query
readQuery = do
  (op : rest) <- words <$> getLine
  return $ case op of
    "1" -> Push (head rest)
    "2" -> Print
    "3" -> Pop

execQuery :: Stack String -> Query -> IO (Stack String)
execQuery stack (Push x) = return $ push stack x
execQuery stack Print = do
  putStrLn $ top stack
  return stack
execQuery stack Pop = return $ pop stack

main = do
  q :: Int <- readLn
  qs <- replicateM q readQuery
  foldM_ execQuery empty qs