module Euler006 where

euler006 = (sum [1..100])^2 - (sum $ map (^2) [1..100])
