module Euler045 where

import Utility

euler045 = putNum $ [t | t <- map tri [285..], isPentagonal t && isHexagonal t]!!1

tri n = n*(n+1)`div`2

pen n = n*(3*n-1)`div`2
isPentagonal x = if 0 < s - fromInteger (floor s)
                   then False
                   else 0 == (truncate s) `mod` 6
                 where s = sqrt (24*(fromInteger x)+1) + 1

hex n = n*(2*n-1)
isHexagonal x = if 0 < s - fromInteger (floor s)
                  then False
                  else 0 == (truncate s) `mod` 4
                where s = sqrt (8*(fromInteger x)+1) + 1
