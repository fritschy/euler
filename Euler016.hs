module Euler016 where
import Utility
euler016 = putNum . sum . digitsOfNumber $ 2^1000
