module Euler012 where
import Utility
euler012 = putStr "False" -- $ factors 48598724
           where factors n = (n : [x | x <- [1..1+n `div` 2], n `mod` x == 0])
