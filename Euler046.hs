module Euler046 where

import Utility
import PrimeNumbers

euler046 = putNum
         . fst
         . head
         $ filter (snd) [(n, null [n | p<-takeWhile (<n) primes, sq<-map (^2) [1..n-1], p+2*sq==n]) | n<-[x | x<-[3,5..], not $ isPrime x]]
