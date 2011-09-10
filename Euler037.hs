module Euler037 where
import Utility
import PrimeNumbers

euler037 = putNum . sum . take 11 $ candidates

candidates = filter (all isPrime . truncated) . drop 4 $ primes

truncated n = filter (/= 0) $ n : a ++ b
              where (a,b) = unzip [n `divMod` (10^x) | x<-[1..(l10 n)]]

l10 = round . logBase 10 . fromIntegral
