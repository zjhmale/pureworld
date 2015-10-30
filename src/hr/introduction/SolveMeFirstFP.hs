module HR.Introduction.SolveMeFirstFP where

solveMeFirst     :: Int -> Int -> Int
solveMeFirst a b = a + b

main :: IO ()
main = do
    val1 <- readLn
    val2 <- readLn
    let sum = solveMeFirst val1 val2
    print sum
