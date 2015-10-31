module HR.Introduction.FilterListPos where

f :: [Int] -> [Int]
f lst = let lstWithIdx = zip [1..] lst
        in do
          elem <- lstWithIdx
          if (fst elem) `mod` 2 == 0
          then return (snd elem)
          else []
    
-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata

