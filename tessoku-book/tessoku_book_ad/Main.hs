module Main (main) where

import Data.ByteString.Char8 (dropWhile, getLine, readInt)
import Data.Functor ((<&>))
import Data.List (unfoldr)
import GHC.Real (denominator, numerator)
import GHC.Unicode (isSpace)
import Prelude hiding (dropWhile, getLine)

-- | 標準入力から空白区切りの整数列を読み込む
ints :: IO [Int]
ints = unfoldr (readInt . dropWhile isSpace) <$> getLine

------------------------------------------------
-- ModInt型：法付きのInt型（modulo に従う）
------------------------------------------------

-- | 法の定数(法は素数)
modulo :: Int
modulo = 10 ^ 9 + 7

newtype ModInt = ModInt {unMod :: Int} deriving (Eq, Ord)

instance Num ModInt where
  (ModInt a) + (ModInt b) = ModInt ((a + b) `mod` modulo)
  (ModInt a) - (ModInt b) = ModInt ((a - b) `mod` modulo)
  (ModInt a) * (ModInt b) = ModInt ((a * b) `mod` modulo)
  negate (ModInt a) = ModInt ((-a) `mod` modulo)
  abs = id
  signum _ = 1
  fromInteger x = ModInt (fromInteger (x `mod` toInteger modulo))

instance Fractional ModInt where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip = inv
  a / b = a * inv b

instance Show ModInt where
  show (ModInt x) = show x

-- 累乗
pow :: ModInt -> Int -> ModInt
pow _ 0 = 1
pow x n
  | even n = pow (x * x) (n `div` 2)
  | otherwise = x * pow x (n - 1)

-- 逆元
inv :: ModInt -> ModInt
inv a = pow a (modulo - 2)

------------------------------------------------
-- Main
------------------------------------------------

-- 階乗テーブル
buildFacts :: Int -> [ModInt]
buildFacts n = scanl (*) 1 (map ModInt [1 .. n])

main :: IO ()
main = do
  [n, r] <- ints
  let facts = buildFacts n
  print $ (facts !! n) / ((facts !! r) * (facts !! (n - r)))