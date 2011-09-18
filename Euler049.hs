module Euler049 where

import Utility
import Data.List
import PrimeNumbers

euler049 = putNum . head $ drop 1 [p*10^8 + (p+i)*10000 + p+i+i | i<-[3000,3002..4000], p<-theprimes i, isPrime (p+i), isPrime (p+i+i), areperms (p+i) (p+i+i), areperms p (p+i)]

theprimes max = takeWhile (<max) $ dropWhile (<1000) primes

areperms x y = (sort . show) x == (sort . show) y
