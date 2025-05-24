module Main (main) where

import Control.Monad (forM_, replicateM)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray, (!))
import Data.Array.ST (STArray, freeze, newArray, readArray, writeArray)
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

buildDP :: Int -> (Array Int Int, Array Int Int) -> Array Int Int
buildDP n (as, bs) = runST $ do
  -- 初期化
  dp <- newArray (1, n) (-(10 ^ 9)) :: ST s (STArray s Int Int)
  writeArray dp 1 0

  -- 変換アクション
  let step i = do
        -- 読み込み
        score <- readArray dp i
        ai <- readArray dp (as ! i)
        bi <- readArray dp (bs ! i)
        -- 漸化式
        let newA = max ai (score + 100)
            newB = max bi (score + 150)
        -- 書き込み
        writeArray dp (as ! i) newA
        writeArray dp (bs ! i) newB

  -- 畳み込み
  mapM_ step [1 .. n - 1]
  -- 終了処理
  freeze dp

main :: IO ()
main = do
  [n] <- ints
  [as, bs] <- replicateM 2 ints <&> map (listArray (1, n - 1))
  let dp = buildDP n (as, bs)
   in print $ dp ! n