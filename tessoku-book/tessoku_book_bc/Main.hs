import Control.Monad (foldM_, replicateM)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

main = do
  n <- readLn
  qs <- replicateM n readQ
  foldM_ execQ S.empty qs

-------------------------------------

data Query = Insert Int | Delete Int | Print Int

readQ = do
  (op : x : rest) <- words <$> getLine
  return $ toQuery op (read x)
  where
    toQuery "1" x = Insert x
    toQuery "2" x = Delete x
    toQuery "3" x = Print x

execQ set (Insert x) = return $ S.insert x set
execQ set (Delete x) = return $ S.delete x set
execQ set (Print x) = do
  print $ fromMaybe (-1) $ S.lookupGE x set
  return set
