module Euler055 where

import Palindrome
import Data.List
import Utility

euler055 = putNum . length $ filter lychrel [10..10000]

lychrel n = lstep' n 0
            where lstep' n c = if c > 50
                                 then True
                                 else if isPalindrome $ show add then False else lstep' add (c+1)
                               where add = n + (read . reverse $ show n)
