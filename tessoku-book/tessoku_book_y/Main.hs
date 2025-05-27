module Main (main) where

import Control.Monad (replicateM)
import Data.Array (Array, Ix, assocs, bounds, listArray, (!))
import Data.Array.Base (amap)
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)
import Prelude hiding (last)

getGrid :: Int -> Int -> IO (Array (Int, Int) Char)
getGrid h w = listArray ((1, 1), (h, w)) . concatMap BS.unpack <$> replicateM h BS.getLine

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

last :: (Ix i) => Array i e -> e
last arr = arr ! snd (bounds arr)

buildDP :: Array (Int, Int) Bool -> Array (Int, Int) Int
buildDP grid = dp
  where
    dp = listArray (bounds grid) (step <$> assocs grid)
    -- 漸化式
    step (_, False) = 0
    step (ij, True) = go ij
      where
        go (1, 1) = 1
        go (1, j) = dp ! (1, j - 1)
        go (i, 1) = dp ! (i - 1, 1)
        go (i, j) = dp ! (i - 1, j) + dp ! (i, j - 1)

main :: IO ()
main = do
  [h, w] <- ints
  grid <- amap (== '.') <$> getGrid h w
  let dp = buildDP grid
  print $ last dp