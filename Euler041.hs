module Euler041 where
import PrimeNumbers
import Utility
import Data.List
-- Only 4 and 7 digit pandigital numbers can be prime:
-- filter snd $ map (\x->(x,(sum [1..x])`mod`3/=0)) [1..9]
-- [(1,True),(4,True),(7,True)]
-- all other are always divisible by 3!
euler041 = putNum $ head [n | n <- [7654321,7654319..4231], isPandigital n && isPrime n]
isPandigital n = [1..length d] == d
                 where d = sort $ digitsOfNumber n
