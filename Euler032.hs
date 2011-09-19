module Euler032 where

import Utility
import Permutation

euler032 :: IO()
euler032 = putNum . sum . sortUniq . map (\(_,_,c)->c) $ pandigitals

pandigitals = filter (\(a,b,c)->a*b==c) . concat . map (split1 . read) $ permutations "123456789"

split1 n = map split2 [divMod n (10^x) | x<-[1..l10 n - 1]]
           where split2 (a,b) = mktpl a b 4
                 mktpl a b e  = (a, x, y)
                                where (x, y) = divMod b (10^e)
