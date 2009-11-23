module Euler012 where
import Utility
euler012 = factors 48598724
--euler012 = head $ filter (\x -> (== 4) . length $ factors (snd x) 2) [(x, sumUpTo x) | x <- [1..]]
           where factors n = (n : [x | x <- [1..1+n `div` 2], n `mod` x == 0])
