module Euler014 where
import Data.List
euler014 = fst $ foldl1 (\a b -> if (snd a) > (snd b) then a else b) [(x,length $ collatz x) | x <- [1..999999]]
           where collatz n
                   | n == 1    = [n]
                   | otherwise = n : collatz (if even n then div n 2 else 3 * n + 1)
                
