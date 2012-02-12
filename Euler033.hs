module Euler033 where

import Data.Ratio
import Utility

euler033 = putNum . numerator $ foldl1 (*) [(x*10+n)%(x+d*10) | n<-[1..9], d<-[1..n-1], x<-[1..9], (x*10+n)%(x+10*d) == n%d]
