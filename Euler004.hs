module Euler004 where
import Data.List
import Palindrome
import Utility
euler004 = putNum . fst $ foldl1 (\x y -> if (fst x) > (fst y) then x else y) lists
           where lists = [(x*y, (x, y)) | x <- items, y <- [x..999], isNumberPalindrome (x*y)]
                 items = [100..999]
