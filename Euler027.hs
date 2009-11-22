module Euler027 where
import PrimeNumbers
euler027 = maximum . map (length . snd) . filter ((> 0) . length . snd) $ map primes candidates
           where candidates = [(a,b) | a <- [-999..999], b <- [-999..999]]
                 primes = \(a,b) -> ((a,b), takeWhile isPrime [n*n+a*n+b | n <- [0..]])
