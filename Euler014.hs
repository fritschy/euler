module Euler014 where
import Data.List
import Utility
euler014 = putNum
         . fst
         $ foldl' (\a b -> if (snd a) > (snd b) then a else b) (0, 0) [(x,length $ collatz x) | x <- [1..999999]]
           where collatz = takeWhile (/= 1) . iterate (\n->if even n then div n 2 else 3 * n + 1)
