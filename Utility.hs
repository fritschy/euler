module Utility where

import Data.Char
import Data.List

digitsOfNumber n = map (sub0 . ord) $ show n
                   where sub0 n = n - 0x30

-- remove adjacent duplicate elements, i.e. only usefull when zthe
-- list is sorted beforehand
uniq (x:xs)
  | not (null xs) = if x == head xs then uniq xs else x : uniq xs
  | otherwise     = [x]
