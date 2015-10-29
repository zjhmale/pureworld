--module Taste (add, sum', qsort) where
--module Taste (add) where
--haskell的module机制是如果像上面一样只标明add那么这个module就只export add这个function 其他都对外部module不可见
module Taste where
import Data.List

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

-- single line comment

{-
multiple
lines
comments
-}

double :: Num n => n -> n
double x = x + x

quadruple :: Num n => n -> n
quadruple x = double $ double x

factorial :: Int -> Int
factorial n = product [1..n]

average :: Fractional a => [a] -> a
average ns = sum ns / genericLength ns

implicit :: Int
implicit = b + c
           where
             b = 1
             c = 2

explicit :: Int
explicit = b + c
           where
             {b = 1;
              c = 2}

cons :: a -> [a] -> [a]
cons x xs = x:xs

conj :: [a] -> a -> [a]
conj xs x = xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isEven :: Integral a => a -> Bool
isEven n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n

-- conditional expressions

abs'   :: Int -> Int
abs' n = if n > 0 then n else -n

signum'   :: Int -> Int
signum' n = if n < 0 then -1 else
             if n == 0 then 0 else 1

-- guarded equations

abs'' :: Int -> Int
abs'' n
  | n >= 0    = n
  | otherwise = -n

signum'' :: Int -> Int
signum'' n
  | n < 0  = -1
  | n == 0    = 0
  | otherwise = 1

-- pattern match

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

null       :: [a] -> Bool
null []    = True
null (_:_) = False
