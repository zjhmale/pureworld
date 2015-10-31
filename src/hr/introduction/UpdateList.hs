module HR.Introduction.UpdateList where

f arr = do
  elem <- arr
  return $ abs elem

-- This section handles the Input/Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata


