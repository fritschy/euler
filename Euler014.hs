module Euler014 where
import Data.List
import Data.Bits
import Utility
euler014 = putNum
         . fst
         $ foldl' (\a b -> if (snd a) > (snd b) then a else b) (0, 0) [(x, collatz x) | x <- [1..999999] :: [Int]]
           where collatz n = c' n 0
                 c' 1 c    = c
                 c' n c    = c' (if n .&. 1 == 0 then shiftR n 1 else 3 * n + 1) (c+1)
