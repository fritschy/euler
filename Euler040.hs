module Euler040 where
import Utility
import Data.Char
euler040 = putNum . product . take 7 . map (digitToInt . snd) . filter ((`elem` [10^n|n<-[0..6]]) . fst) . zip [1..] . concat $ map show [1..]
