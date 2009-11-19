module Euler001 where

euler001 = sum [x | x <- [2..999], x `mod` 5 == 0 || x `mod` 3 == 0]
