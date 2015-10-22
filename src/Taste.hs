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
