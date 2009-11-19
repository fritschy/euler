module Euler004 where
import Data.List
import Palindrome
euler004 = head $ sortBy srtf lists
           where lists = [(x*y, (x, y)) | x <- items, y <- items, isNumberPalindrome (x*y)]
                 items = [100..999]
                 srtf x y
                   | (fst x) < (fst y) = GT
                   | (fst x) > (fst y) = LT
                   | otherwise         = EQ
