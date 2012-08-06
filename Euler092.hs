module Euler092 where
import Utility
euler092 = putNum . length $ filter l [1..(10000000-1)]
  where l 89 = True
        l  1 = False
        l  n = l (sum . map (^2) $ digitsOfNumber n)
