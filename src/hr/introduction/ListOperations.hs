module HR.Introduction.ListOperations where

nelements n = replicate n n

len        :: [a] -> Int
len []     = 0
len (x:xs) = len xs + 1
              
rev        :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]
