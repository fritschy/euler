module PrimeNumbers (
  sieve,
  isPrime,
  primes,
  primeFactors
  ) where

import Data.Bits

sieve :: Integral a => [a]
sieve = [x | x <- [2..], null [y | y <- [2..truncate . sqrt $ fromIntegral x], x `mod` y == 0]]

isPrime :: Integral a => a -> Bool
isPrime n
  | n == 2    = True
  | n >  6    = null [x | x <- [2 .. max], 0 == mod n x]
  | otherwise = False
                where max = truncate (1 + sqrt (fromIntegral n))

primes :: Integral a => [a]
primes = [x | x <- 2:[3,5..], isPrime x]

primeFactors :: Integral a => a -> [a]
primeFactors n
  | n == 1    = []
  | otherwise = p : primeFactors (div n p)
                where p = head [x | x <- primes, 0 == mod n x]
