import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List (sortBy, unfoldr)
import Data.Ord (comparing)
import GHC.Unicode (isSpace)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 = ints <&> \[x1, x2] -> (x1, x2)

solve n movies = snd $ foldl step (0, 0) movies
  where
    step (now, nWatechedMovie) (l, r)
      | now <= l = (r, nWatechedMovie + 1)
      | otherwise = (now, nWatechedMovie)

main :: IO ()
main = do
  -- 入力
  [n] <- ints
  movies <- replicateM n ints2 <&> sortBy (comparing snd)
  -- 出力
  print $ solve n movies