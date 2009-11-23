module Euler023 where
import Utility
euler023 = putNum 0 -- . sum $ filter findSum [1..28123-1]
           where abundants = filter ((== Abundant) . classify) numbers
                 numbers   = [x | x <- [2..28123-1]]
                 findSum r = null [a | a <- abundants, (r - a) `elem` abundants]
