module Euler058 where
import Utility
import Debug.Trace
import PrimeNumbers
-- walk the spiral around and around adding numbers which we find on the diagonal
spiral = fourSteps 2 3 1 0 1
           where fourSteps stepSize stepsToGo num ps cs
                   -- | trace (show num ++ ", " ++ show ps ++ ", " ++ show cs ++ ", " ++ show rat) False = undefined
                   -- this means we circled around 3 corners and need to lengthen the stepSize as
                   -- we are about to start a new layer, also, we are on a diagonal
                   | ps > 0 && cs > 0 && rat < 0.1 = stepSize-1
                   | stepsToGo == 0                = fourSteps (stepSize+2) 3 (num+stepSize) nps ncs
                   -- finish iteration...
                   -- at a diagonal, decrease the corners we need to turn (stepsToGo)
                   | otherwise                     = fourSteps stepSize (stepsToGo-1) (num+stepSize) nps ncs
                     where (nps, ncs) = if isPrime num then (ps+1, cs+1) else (ps, cs+1)
                           rat        = (fromIntegral ps / fromIntegral cs)

euler058 = putNum $ spiral
