module Euler030 where
import Utility
euler030 = putNum . sum $ filter (\x -> x == (sum $ map (^5) (digitsOfNumber x))) [2..295245]
