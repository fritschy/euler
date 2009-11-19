module Euler020 where
import Utility
euler020 = sum . digitsOfNumber $ product [2..100]
           where sub0 x = x - 0x30
