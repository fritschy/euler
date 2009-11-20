module Utility where

import Data.Bits
import Data.Char
import Data.List

digitsOfNumber n = map (sub0 . ord) $ show n
                   where sub0 n = n - 0x30

-- remove adjacent duplicate elements, i.e. only usefull when zthe
-- list is sorted beforehand
uniq (x:xs)
  | not (null xs) = if x == head xs then uniq xs else x : uniq xs
  | otherwise     = [x]

-- sum the numbers from 1..n
sumUpTo n = n * (n + 1) `div` 2
--sumUpTo n = n + n * (n - 1) `div` 2

showBinary x
  | x > 0     = (if testBit x 0 then '1' else '0') : showBinary (x `div` 2)
  | otherwise = []
