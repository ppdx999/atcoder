module Main (main) where

import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)
import Prelude hiding (Double)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

solve :: [Int] -> [Int] -> [Int]
solve (a2 : as) bs =
  let -- 初期状態
      room1 = (0, [1])
      room2 = (a2, [2, 1])

      -- 漸化式
      --   c1 :: Int      -->   １つ前のコスト
      --   p1 :: [Int]    -->   １つ前までの経路
      --   c2 :: Int      -->   ２つ前のコスト
      --   p2 :: [Int]    -->   ２つ前までの経路
      calcNext ((c1, p1), (c2, p2)) (i, a, b) =
        min (c1 + a, i : p1) (c2 + b, i : p2)

      -- ステップ
      step acm curr = (calcNext acm curr, fst acm)
      -- 畳み込み
      (_, revPath) = fst $ foldl step (room2, room1) (zip3 [3 ..] as bs)
   in -- 終了値
      reverse revPath

main :: IO ()
main = do
  [n] <- ints
  [as, bs] <- replicateM 2 ints
  let path = solve as bs
  print $ length path
  putStrLn $ unwords (map show path)
