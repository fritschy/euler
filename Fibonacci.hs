module Fibonacci where
import Data.Function
fibs0 = fix ((0:) . (scanl (+) 1))
fibs  = tail $ fibs0
