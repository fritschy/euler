module Euler050 where

import Utility
import PrimeNumbers
import Data.List

-- as usual, magic numbers are trial-and-error
euler050 = putNum . dfst . head . sortBy (\((_,x),_) ((_,y),_)->compare y x) . filter (isPrime . dfst) $ filter ((<1000000) . dfst) [prms d t | t<-[500..700], d<-[0..10]]

dfst = fst . fst
prms d t = ((sum p, length p), p) where p = take t $ drop d primes
