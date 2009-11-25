module Euler020 where
import Utility
euler020 = putNum . sum . digitsOfNumber $ product [2..100]
