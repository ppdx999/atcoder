import Control.Monad (foldM_, replicateM)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Prelude hiding (reverse)

---------------------------------
data Array e = Array
  { len :: Int,
    isReverse :: Bool,
    m :: M.Map Int e
  }

-- 配列の状態を見て正しいIndexを返却する
idx arr i =
  if isReverse arr
    then len arr - i + 1
    else i

-- | 計算量O(logN)でArrayの要素iにアクセスする(範囲外のアクセスはエラー)
(!) :: Array e -> Int -> e
(!) arr i = fromMaybe (error "Array (!) out of bounds") (M.lookup (arr `idx` i) (m arr))

-- | 計算量O(logN)でArrayの要素を更新する
(//) :: Array e -> (Int, e) -> Array e
(//) arr (i, a) =
  Array (len arr) (isReverse arr) (M.insert (arr `idx` i) a (m arr))

-- | 配列を論理的に反転させる
reverse :: Array e -> Array e
reverse arr = Array (len arr) (not $ isReverse arr) (m arr)

-- | 要素がすべて同じArray eを返す
listArray :: Int -> [e] -> Array e
listArray n as = Array n False (M.fromList (zip [1 .. n] as))

---------------------------------
data Query = Replace Int Int | Reverse | Print Int

-- Queryを入力から読み込む
readQuery :: IO Query
readQuery = do
  (op : rest) <- getLine
  return $ case op of
    '1' -> let [x, y] = map read . words $ rest in Replace x y
    '2' -> Reverse
    '3' -> Print (read rest)

-- Queryを実行する
executeQuery :: Array Int -> Query -> IO (Array Int)
executeQuery arr (Replace x y) = return $ arr // (x, y)
executeQuery arr Reverse = return $ reverse arr
executeQuery arr (Print x) = do
  print $ arr ! x
  return arr

---------------------------------

main = do
  -- 入力
  [n, q] <- map read . words <$> getLine
  queries <- replicateM q readQuery
  let arr = listArray n [1 .. n]
  -- 出力
  foldM_ executeQuery arr queries