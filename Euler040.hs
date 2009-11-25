module Euler040 where
import Utility
euler040 = putNum . product . take 7 . map (read . (:[]) . snd) . filter ((`elem` [10^n|n<-[0..6]]) . fst) . zip [1..] . concat $ map show [1..]
