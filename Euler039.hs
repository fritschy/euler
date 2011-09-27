module Euler039 where

import Utility
import Data.List

euler039 = putNum
         . snd
         . head
         . sortBy (\(x,_) (y,_)->compare y x)
         . map (\(x,y)->(length y, x))
         $ filter (not . null . snd) lst
         where lst = [(d, [(x,y,d-x-y)|x<-[3..d-2], y<-[4,6..x-1], (d-x-y)^2==x^2+y^2]) | d<-[24,28..1000]]
