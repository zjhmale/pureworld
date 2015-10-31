module HR.Introduction.SumOfOddElements where

f arr = let oddlist = [a | a <- arr, a `mod` 2 /= 0]
        in sum oddlist

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
   inputdata <- getContents
   putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata
