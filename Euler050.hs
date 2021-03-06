module Euler050 where

import Utility
import PrimeNumbers
import Data.List

-- as usual, magic numbers are trial-and-error
euler050 = putNum
         . fst
         . head
         . sortBy (\(_,x) (_,y)->compare y x)
         . filter (isPrime . fst)
         $ filter ((<1000000) . fst) [prms d t | t<-[500..700], d<-[0..10]]

prms d t = (sum p, length p) where p = take t $ drop d primes
