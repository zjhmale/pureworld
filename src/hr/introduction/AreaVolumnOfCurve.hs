{-# LANGUAGE FlexibleContexts #-}
module HR.Introduction.AreaVolumnOfCurve where

import Text.Printf (printf)

-- 题目的意思是给出一条二维曲线的表达式然后求出其某一段范围内和x轴围成的几何图形的面积以及绕x轴旋转得到立体图形的体积    
-- preferences
-- http://www.intmath.com/applications-integration/4-volume-solid-revolution.php
-- http://tutorial.math.lamar.edu/Classes/CalcI/Area_Volume_Formulas.aspx
    
calcy :: Double -> [Int] -> [Int] -> [Double]
calcy x a b = let lst = do 
                    param <- zip a b
                    return $ fromIntegral (fst param) * x ** fromIntegral (snd param)
               in [sum lst]

calcArea :: [Double] -> [Int] -> [Int] -> Double
calcArea vals a b = let lst = do
                          val <- vals
                          y <- calcy val a b
                          return $ 0.001 * y
                    in sum lst

calcVolumn :: [Double] -> [Int] -> [Int] -> Double
calcVolumn vals a b = let lst = do
                            val <- vals
                            y <- calcy val a b
                            return $ pi * y ^ 2 * 0.001
                      in sum lst    
               
-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = let vals = [fromIntegral l, fromIntegral l + 0.001 .. fromIntegral r]
                in [calcArea vals a b, calcVolumn vals a b]
                   
--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
