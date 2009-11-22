module Euler027 where
import PrimeNumbers
euler027 = foldl1 (\a b -> if (l a) < (l b) then b else a) $ map primes candidates
           where candidates = [(a,b) | a <- [-999..999], b <- [-999..999]] -- odd b?
                 primes (a,b) = (((a, b), a*b), takeWhile isPrime [n*n+a*n+b | n <- [0..]])
                 l = length . snd
