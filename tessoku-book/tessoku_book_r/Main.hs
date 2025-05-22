module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import GHC.Unicode (isSpace)

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

safeIndexOp :: [Bool] -> Int -> Bool
safeIndexOp t a
  | a >= 0 = t !! a
  | otherwise = False

(!!!) = safeIndexOp

-- 漸化式
-- dp[i+1][j] = dp[i][j] || dp[i][j-A[i]]
-- s.t. dp[i][j] ... i枚目までのカードで合計jを作ることができる

-- stepはdp[i]をdp[i+1]にマッピングする
step :: Int -> [Bool] -> [Bool]
step card_i dp_i = do
  j <- [0 .. (length dp_i - 1)]
  return $ (dp_i !!! j) || (dp_i !!! (j - card_i))

main :: IO ()
main = do
  [n, s] <- ints
  cards <- ints
  let dp0 = True : replicate s False
      dp = foldr step dp0 cards
   in printYn $ last dp