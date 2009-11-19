module Euler007 where
import PrimeNumbers
euler007 = [x | x <- [2..], null [y | y <- [2..truncate . sqrt $ fromIntegral x], x `mod` y == 0]]!!10000
