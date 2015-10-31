module HR.Introduction.EvaluatingEX where

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

solve   :: Double -> Double
solve x = let lst = do
                e <- [0..9]
                return $ x ^ e / (fromInteger $ factorial e :: Double)
          in sum lst

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words
