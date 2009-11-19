module Euler029 where
import Data.List
euler029 = length . uniq $ sort [a^b | a <- [2..100], b <- [2..100]]
           where uniq (x:xs)
                   | xs /= []  = if x == head xs then uniq xs else x : uniq xs
                   | null xs   = [x]
                   | otherwise = []
