module Euler048 where
import Utility
euler048 = (`mod` 10^10) $ sum [x^x | x <- [1..1000]]
