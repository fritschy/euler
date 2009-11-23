module Euler016 where
import Data.Char
import Utility
euler016 = putNum . sum . map (sub30 . ord) . show $ 2^1000
           where sub30 n = n - 0x30
