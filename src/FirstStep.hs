module FirstStep where
import Data.List

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

