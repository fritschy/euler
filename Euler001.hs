module Euler001 where
import Utility
euler001 = putNum $ sum [x | x <- [3..999], x `mod` 5 == 0 || x `mod` 3 == 0]
