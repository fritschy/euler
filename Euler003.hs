module Euler003 where
import PrimeNumbers
import Utility
euler003 = putNum . maximum $ primeFactors 600851475143
