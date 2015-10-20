--module Taste (add, sum', qsort) where
--module Taste (add) where
--haskell的module机制是如果像上面一样只标明add那么这个module就只export add这个function 其他都对外部module不可见
module Taste where

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]


