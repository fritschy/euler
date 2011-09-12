module Euler012 where
import Utility
euler012 = putNum . fst . head . filter ((> 500) . length . snd) $ zip a b
           where a = map sumUpTo [1..]
                 b = map (divisors . sumUpTo) [1..]
