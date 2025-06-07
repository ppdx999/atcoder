import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.Functor ((<&>))

modulo, base :: Int
modulo = 2147483647
base = 100

main :: IO ()
main = do
  [n, q] <- ints
  [s] <- words <$> getLine
  qs <- replicateM q ints4
  let hasher = getHasher (buildHash s)
      answer (l1, r1, l2, r2) = hasher (l1 - 1) r1 == hasher (l2 - 1) r2
  mapM_ (printYn . answer) qs

-- 与えられた区間 [l, r) のハッシュを取得
getHasher :: (Array Int Int, Array Int Int) -> Int -> Int -> Int
getHasher (hash, powBase) l r =
  let h = (hash ! r - (hash ! l * powBase ! (r - l)) `mod` modulo) `mod` modulo
   in if h < 0 then h + modulo else h

-- 文字列から prefix hash と base のべき乗列を構築
buildHash :: String -> (Array Int Int, Array Int Int)
buildHash s = (prehash, powBase)
  where
    n = length s
    codes = map fromEnum s
    prehash = listArray (0, n) $ scanl (\h c -> (h * base + c) `mod` modulo) 0 codes
    powBase = listArray (0, n) $ scanl (\p _ -> (p * base) `mod` modulo) 1 [1 .. n]
    hasher l r =
      let h = (prehash ! r - (prehash ! l * powBase ! (r - l)) `mod` modulo) `mod` modulo
       in if h < 0 then h + modulo else h

------------------------
-- 入出力
------------------------
ints :: IO [Int]
ints = map read . words <$> getLine

ints4 :: IO (Int, Int, Int, Int)
ints4 = ints <&> (\[x1, x2, x3, x4] -> (x1, x2, x3, x4))

yn :: Bool -> String
yn True = "Yes"
yn False = "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn
