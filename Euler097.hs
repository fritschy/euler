module Euler097 where
euler097 = putStr . reverse . take 10 . reverse $ show (28433*2^7830457+1)
