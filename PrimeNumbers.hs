module PrimeNumbers (
  sieve,                -- Integral a => [a]
  isPrime,              -- Integral a => a -> Bool
  primes,               -- Integral a => [a]
  primeFactors          -- Integral a => a -> [a]
  ) where

import Utility
import Data.Bits

sieve :: Integral a => [a]
sieve = [x | x <- [2..], null [y | y <- [2..factorLimit x], x `mod` y == 0]]

isPrime :: Integral a => a -> Bool
isPrime n
  | n == 2         = True
  | n `mod` 2 == 0 = False
  | n >  2         = null [x | x <- [3,5 .. max], 0 == mod n x]
  | otherwise      = False
                     where max = 1 + factorLimit n

primes :: Integral a => [a]
primes = 2:[x | x <- [3,5..], isP x]
         where isP n = null [x | x <- [3,5 .. 1 + factorLimit n], 0 == mod n x]

primeFactors :: Integral a => a -> [a]
primeFactors n
  | n == 1    = []
  | otherwise = p : primeFactors (div n p)
                where p = head [x | x <- (takeWhile ((<= n) . square) primes) ++ [n], 0 == mod n x]

square :: Num a => a -> a
square x = x * x

factorLimit :: Integral a => a -> a
factorLimit = truncate . sqrt . fromIntegral
