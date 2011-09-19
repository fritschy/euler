module Euler043 where

import Data.List
import Utility
import PrimeNumbers

euler043 = putNum . sum . map (\(x,_)->sum . map (\(t,n)->n*10^t) . zip [0..] $ reverse x) . filter (\(a,b)->all (\(y,x)->y `divides` x) b) . map (\x->(x,zip primes . nums $ tail x)) $ permutations [0..9]

nums :: [Int] -> [Int]
nums (a:b:c:xs) = a*100+b*10+c : if not $ null xs then nums (b:c:xs) else []
