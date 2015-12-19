module HR.Recursion.StringReduction where

unique :: [Char] -> [Char]
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]

main :: IO ()
main = do
    str <- getLine
    putStrLn $ unique str
