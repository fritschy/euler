module Euler047 where

import PrimeNumbers
import Utility

euler047 = putStr . show . fst . head . four . filter ((==4) . length . uniq . snd) $ map (\x->(x,primeFactors x)) [2..]
           where four (x@(a,_):y@(b,_):z@(c,_):w@(d,_):xs) = if (a+1)==b && (b+1)==c && (c+1)==d then [x,y,z,w] else four (y:z:w:xs)
