module Euler002 where
import Fibonacci
euler002 = sum . takeWhile (<= 4000000) . filter even $ fibs
