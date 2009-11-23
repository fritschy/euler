module Euler002 where
import Fibonacci
import Utility
euler002 = putNum . sum . takeWhile (<= 4000000) . filter even $ fibs
