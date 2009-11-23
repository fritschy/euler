module Euler021 where
import Utility
euler021 = foldl (\a (x,y) -> a+x+y) 0 $ uniq [srt (x, y) | x <- nums, y <- nums, x /= y && (sum (divisors x) == y) && (sum (divisors y) == x)]
           where srt (x, y) = if x < y then (x, y) else (y, x)
                 nums = [1..10000]