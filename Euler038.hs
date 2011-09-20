module Euler038 where

import Data.Char
import Data.List
import Utility

euler038 = putNum . (\(_,_,x)->x) . head . sortThem $ map c2n cand

sortThem = sortBy (\(_,_,a) (_,_,b) -> compare b a) . filter (\(_,_,x) -> isPandigital x && x<10^9 && x>99999999)

c2n :: (Integer, [Integer]) -> (Integer, Int, Integer)
c2n (x,ns) = (x, length ns, read . concat . map show $ map (*x) ns)

cand :: [(Integer,[Integer])]
cand = [(x,[1..mn]) | x <- [9000..10000], mn <- [2..3]]
