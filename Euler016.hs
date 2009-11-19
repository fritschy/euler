module Euler016 where
import Data.Char
euler016 = sum . map (sub30 . ord) . show $ 2^1000
           where sub30 n = n - 0x30
