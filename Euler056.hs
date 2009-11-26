module Euler056 where
import Utility
euler056 = putNum $ maximum [sum $ digitsOfNumber (a^b) | a <- [1..99], b <- [1..99]]
