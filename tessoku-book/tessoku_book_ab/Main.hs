module Main (main) where

import Control.Monad (foldM_, replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Data.List qualified as L
import GHC.Unicode (isSpace)

-- | 標準入力の1行をスペース区切りで文字列のリストに分割する
wordsBS :: IO [String]
wordsBS = map BS.unpack . BS.words <$> BS.getLine

-- | 標準入力の1行をスペース区切りで整数のリストに変換する
ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | (演算子, 数値) のコマンド型
type Cmd = (Char, Int)

-- | 標準入力からコマンドを1行読み取る（例: "+ 5"）
readCmd :: IO Cmd
readCmd = do
  [[op], n] <- wordsBS
  return (op, read n)

-- | 値を出力してそのまま返すユーティリティ関数
out :: (Show a) => a -> IO a
out a = print a >> return a

-- | 畳み込みのステップ関数とログ出力関数を合成する演算子
(<=>) :: (b -> IO b) -> (b -> a -> b) -> (b -> a -> IO b)
(<=>) logger step b a = logger (step b a)

-- | コマンドを実行する
exec :: Cmd -> Int -> Int
exec ('+', a) x = x + a
exec ('-', a) x = x - a
exec ('*', a) x = x * a
exec _ _ = error "invalid command"

-- | 正の数だけを 10000 で割った余りを返す
--   負の値も正の範囲（0〜9999）に収める
mod10000 :: Int -> Int
mod10000 x
  | x >= 0 = x `mod` 10000
  | otherwise = x + 10000

-- | 畳み込みの各ステップの計算
step :: Int -> Cmd -> Int
step x cmd = mod10000 $ exec cmd x

-- | Main
main :: IO ()
main = do
  [n] <- ints
  cmds <- replicateM n readCmd
  foldM_ (out <=> step) 0 cmds
