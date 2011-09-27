module Euler026 where
import Utility
import Data.List
euler026 = putNum . fst . snd . head $ work
work = sortBy (\(a,_) (b,_)->compare b a)
     . filter ((< 2999) . fst)
     $ map (\x->let ss = substr $ show (10^3000 `div` x) in (length ss, (x, ss))) [750..999]
substr s = f (tail s, [head s])
           where f (s, ss) = if null s
                               then []
                               else if take ssl s /= ss
                                 then f (tail s, ss ++ [head s])
                                 else ss
                             where ssl = length ss
