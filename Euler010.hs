module Euler010 where
import PrimeNumbers
import Utility
euler010 = putNum . sum $ takeWhile (< 2000000) primes
