module Euler053 where
import Utility
euler053 = putNum . length $ filter (>10^6) [n `over` r | n<-[1..100], r<-[1..n]]
