module Euler025 where
import Fibonacci
import Utility
euler025 = putNum $ head [x | (y,x) <- zip fibs [1..], 10^999 <= y]
