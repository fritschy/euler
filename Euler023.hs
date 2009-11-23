module Euler023 where
import Utility
euler023 = abundants
           where abundants = filter ((== Abundant) . classify) numbers
                 numbers   = [x | x <- [2..28123-1]]
