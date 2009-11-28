module Euler012 where
import Utility
euler012 = putNum . fst . head . filter ((> 500) . length . snd) . zip [1..] $ map (divisors . sumUpTo) [1..]
