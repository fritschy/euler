module Euler028 where

-- walk the spiral around and around adding numbers which we find on the diagonal
spiral n = fourSteps 2 3 1
           where fourSteps stepSize stepsToGo num
                   | stepsToGo == 0 = num + fourSteps (stepSize+2) 3 (num+stepSize)
                   | num == n       = num
                   | otherwise      = num + fourSteps stepSize (stepsToGo-1) (num+stepSize)

euler028 = spiral (1001*1001)
