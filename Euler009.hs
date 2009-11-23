module Euler009 where

euler009 = product abc
           where abc = head $ [[x, y, z] | x <- [3,6..1000], y <- [4,8..1000], z <- [5,10..1000], cond x y z]
                 cond x y z = x*x + y*y == z*z && x + y + z == 1000
