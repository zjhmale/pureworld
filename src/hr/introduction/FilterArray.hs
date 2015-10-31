module HR.Introduction.FilterArray where

f :: Int -> [Int] -> [Int]
f n arr = do
  elem <- arr
  if elem < n
  then return elem
  else []

-- The Input/Output section. You do not need to change or modify this part
main = do 
    n <- readLn :: IO Int 
    inputdata <- getContents 
    let 
        numbers = map read (lines inputdata) :: [Int] 
    putStrLn . unlines $ (map show . f n) numbers

