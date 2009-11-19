module Euler014 where
import Data.List
euler014 = head . sortBy srtf $ [(x,length $ collatz x) | x <- [999999,999998..1]]
           where srtf x y
                   | (snd x) < (snd y) = GT
                   | (snd x) > (snd y) = LT
                   | otherwise         = EQ
                 collatz n
                   | n == 1    = [n]
                   | otherwise = n : collatz (if even n then div n 2 else 3 * n + 1)
