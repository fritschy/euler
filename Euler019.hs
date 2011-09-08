module Euler019 where

import Utility

-- 1900.01.01 was a monday
-- 30 days: april, june, september, november
-- 28 days: February, 29 in leap years
-- 31 days: january, march, mai, july, august, october, december
-- leap year is every year divisible by 4, but not divisible by 100 unless it is divisible by 400
-- how many sundays fell on a first of the month during the 20th century
-- (1901.01.01 .. 2000.12.31)

euler019 = putNum . length $ iterateDays (1, 1, 1900) (31, 12, 2000) 0

iterateDays c@(cd,cm,cy) e dow
  | e /= c    = if inc then c : cont else cont
  | otherwise = []
    where cont  = iterateDays next e ((dow+1) `mod` 7)
          inc   = cd == 1 && dow == 6 && cy > 1900
          lastD = if cm `elem` [1,3,5,7,8,10,12]
                    then 31
                    else if cm `elem` [4,6,9,11]
                      then 30
                      else if leapY then 29 else 28
          next  = if cd == lastD
                      then if cm == 12
                        then (1,1,cy+1)
                        else (1,cm+1,cy)
                      else (cd+1,cm,cy)
          leapY = cy `mod` 4 == 0 && (cy `mod` 100 /= 0 || cy `mod` 400 == 0)
