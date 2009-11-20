module Euler012 where
import Utility
euler012 = head [x | x <- map sumUpTo [1..], (5 <) . length $ factors x]
           where factors n = 1 : (n : [x | x <- [2..n `div` 2], n `mod` x == 0])
