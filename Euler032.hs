module Euler032 where

import Utility
import Permutation

euler032 = putNum . sum . sortUniq . map (\(a,b,c)->c) $ pandigitals

pandigitals = filter (\(a,b,c)->a*b==c) . concat . map (split1 . read) $ permutations "123456789"

split1 n = map split2 [divMod n (10^x) | x<-[1..ilog10 n]]
           where split2 (a,b) = mktpl a b 4
                 ilog10 n = (round . logBase 10 $ fromIntegral n) - 1
                 mktpl a b e = (a, x, y)
                               where (x, y) = divMod b (10^e)
