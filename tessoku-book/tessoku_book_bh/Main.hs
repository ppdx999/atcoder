main = input >>= output . solve

solve (n, as) = reverse $ snd $ foldl step ([], []) (zip [1 .. n] as)
  where
    -- Level1, Level2の定義を知りたい方は鉄則本をご確確ください
    step (lev2Stack, ans) (date, price) =
      let s = rmLev1 lev2Stack
       in (addLev2 s, fst (top s) : ans)
      where
        addLev2 stack = push stack (date, price)
        rmLev1 stack =
          if price >= snd (top stack)
            then rmLev1 (pop stack)
            else stack

-------------------------------
-- In / Out
input = do
  n <- read <$> getLine
  as <- map read . words <$> getLine
  return (n, as)

output = putStrLn . unwords . map show

-------------------------------
-- Stack(日付, 価格)
type Stack = [(Int, Int)]

pop :: Stack -> Stack
pop [] = []
pop (x : xs) = xs

top :: Stack -> (Int, Int)
top [] = (-1, maxBound)
top (x : xs) = x

push :: Stack -> (Int, Int) -> Stack
push xs x = x : xs