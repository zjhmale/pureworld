solve :: Int -> Int -> [Double]
solve l r = [fromIntegral l, fromIntegral l + 0.001 .. fromIntegral r]
