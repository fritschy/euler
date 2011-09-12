module Euler035 where
import PrimeNumbers
import Utility
import Data.Char
euler035 = putNum . length . filter cprime . filter noEven $ takeWhile (< 1000000) primes
           where noEven n = n < 10 || (null . filter (even . digitToInt) $ show n)

cprime n = and [isPrime . read $ rot sn x | x <- [0..length sn]]
           where sn = show n

rot l n = a ++ b
          where (b,a) = splitAt n l

-- a little faster, but complex...
--rot (x:xs) n = if n == 0 then x:xs else rot (xs ++ [x]) (n-1)
