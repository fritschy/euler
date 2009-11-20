module Euler025 where
import Fibonacci
euler025 = head [x | (y,x) <- zip fibs [1..], 1000 <= (length $ show y)]
