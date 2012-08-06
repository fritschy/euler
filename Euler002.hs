module Euler002 where
import Fibonacci
import Utility
euler002 = putNum
         . sum
         . filter even
         $ takeWhile (<= 4000000) fibs
