module Euler044 where

import Utility
import Data.List
import qualified Data.Set as S

euler044 = putNum $ head [a - b | a<-ps, b<-ps, isp (a+b), isp (a-b)]
           where ps = take 2500 pentagonals
                 sps = S.fromList ps
                 isp = (flip S.member) sps
