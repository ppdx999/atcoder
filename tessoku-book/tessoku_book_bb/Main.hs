import Control.Monad (foldM_, replicateM)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

main = do
  n <- readLn
  qs <- replicateM n readQ
  foldM_ execQ M.empty qs

----------------------------------

data Q = Register String Int | Print String

readQ = do
  (op : rest) <- words <$> getLine
  return $ case op of
    "1" -> (\[x, y] -> Register x (read y)) rest
    "2" -> Print (head rest)

execQ map (Register x y) = return $ M.insert x y map
execQ map (Print x) = do
  print $ fromMaybe (error "") $ M.lookup x map
  return map