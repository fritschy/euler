module Euler004 where
import Data.List
import Palindrome
euler004 = fst $ foldl1 (\x y -> if (fst x) > (fst y) then x else y) lists
           where lists = [(x*y, (x, y)) | x <- items, y <- items, isNumberPalindrome (x*y)]
                 items = [100..999]
