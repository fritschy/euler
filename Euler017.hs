module Euler017 where

import Utility
import Array
import Data.Char

oneToNine :: Int -> String
oneToNine n
  | n >= 1 && n <= 9 = chars!(n-1)
  | otherwise        = error $ "Number not in range 1..9 given to oneToNine: " ++ show n
                       where chars = array (0,8) [(0, "one"), (1, "two"), (2, "three"), (3, "four"), (4, "five"),
                                                  (5, "six"), (6, "seven"), (7, "eight"), (8, "nine")]

oneToNine10 :: Int -> String
oneToNine10 n
  | n >= 2 && n <= 9 = chars!(n-1)
  | otherwise        = error $ "Number not in range 2..9 given to oneToNine10: " ++ show n
                       where chars = array (0,8) [(0, "one"), (1, "twenty"), (2, "thirty"), (3, "forty"), (4, "fifty"),
                                                  (5, "sixty"), (6, "seventy"), (7, "eighty"), (8, "ninety")]

teens :: Int -> String
teens n
  | n >= 10 && n <= 19 = chars!(n-10)
  | otherwise          = error $ "Number not in range 1..9 given to teens: " ++ show n
                         where chars = array (0,9) [(0, "ten"), (1, "eleven"), (2, "twelve"), (3, "thirteen"), (4, "fourteen"),
                                                    (5, "fifteen"), (6, "sixteen"), (7, "seventeen"), (8, "eighteen"), (9, "nineteen")]

showLiteralNumber n
  | n == 0                 = []
  | n >= 1000 && n <= 9999 = ((oneToNine (head digits)) ++ " thousand ") ++ showLiteralNumber (next 1)
  | n >=  100 && n <=  999 = ((oneToNine (head digits)) ++ " hundred " ++ (if (next 1) == 0 then [] else "and ")) ++ showLiteralNumber (next 1)
  | n >=   20 && n <=   99 = (oneToNine10 (head digits)) ++ showLiteralNumber (next 1)
  | n >=   10 && n <=   19 = (teens ((head digits)*10 + (head $ tail digits))) ++ showLiteralNumber (next 2)
  | n >=    1 && n <=    9 = (oneToNine (head digits)) ++ showLiteralNumber (next 1)
                             where digits = digitsOfNumber n
                                   next x = read . ("0" ++) . concat . map show . (if x == 2 then tail else id) $ tail digits

euler017 = length $ concat [concat . words $ showLiteralNumber n | n <- [1..1000]]
