module Euler092 where
import Utility
euler092 = putNum . length $ filter (\x->89 == l x) [1..(10000000-1)]

l :: Int -> Int
l n = if n == 89 || n == 1
        then n
        else l (sum . map (^2) $ digitsOfNumber n)
