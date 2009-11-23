module Euler006 where
import Utility
euler006 = putNum ((sumUpTo 100)^2 - (sum $ map (^2) [1..100]))
