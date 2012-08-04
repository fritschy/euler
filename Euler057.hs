module Euler057 where

import Utility
import Data.Ratio

data CF = CF Integer Integer (Integer, Integer)
  deriving (Show, Eq)

euler057 = putNum . length $ filter p [e (CF 1 x (1,2)) | x <- [8..1000]]
  where p x = l10 (numerator x) > l10 (denominator x)

e (CF a n bc) = a % 1 + e2 n bc
e2 n ab@(a, b)
  | n == 1    = a % b
  | otherwise = (a % 1) / (b % 1 + (e2 (n-1) ab))
