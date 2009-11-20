module Euler027 where
import PrimeNumbers
euler027 = "Not yet solved..."
--euler027 = map showIt candidates
           where candidates = [(a,b) | a <- [-999..999], b <- [-999..999], (max a b) - 1 == length [n | n <- [0..(max a b)-1], n*n+a*n+b > 0 && isPrime (n*n+a*n+b)]]
                 showIt (a, b) = "n^2  +  " ++ show a ++ " * n  +  " ++ show b
