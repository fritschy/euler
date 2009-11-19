module Euler010 where
import PrimeNumbers
euler010 = sum $ takeWhile (< 2000000) primes
