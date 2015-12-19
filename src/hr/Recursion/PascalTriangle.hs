module HR.Recursion.PascalTriangle where
import Control.Applicative ((<$>))

pascal :: [[Int]]
pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

flatStr :: [Int] -> String
flatStr = unwords . map show

main :: IO ()
main = do
    lineNum <- readLn
    mapM_ putStrLn $ flatStr <$> take lineNum pascal
