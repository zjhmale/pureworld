module HR.Recursion.SierpinskiTriangles where

sumPairs :: [Integer] -> [Integer]
sumPairs (x:y:s) = (x + y) : sumPairs (y:s)
sumPairs _       = []

pascal :: Integer -> [Integer]
pascal 0 = [1]
pascal n = sumPairs $ [0] ++ (pascal $ n - 1) ++ [0]

sierpinski :: Integer -> String
sierpinski n = concat $ map (ascii . odd) $ pascal n
               where ascii True  = "1"
                     ascii False = "_"

sierpinskiTriangle :: Integer -> [String]
sierpinskiTriangle n = map (centre . sierpinski) [0..n]
                       where centre line = (spaces padding) ++ line
                                           where padding = 1 + (fromIntegral n) - ((length line) `div` 2)
                             spaces m    = take m [' ',' '..]
