module Fibonacci(
  fibs0,
  fibs,
  fibWords
  ) where

import Control.Monad.Fix -- works with hugs and ghc (hugs doesn't know Data.Function)

fibs0, fibs :: Integral a => [a]
fibs0 = fix ((0:) . (scanl (+) 1))
fibs  = tail $ fibs0

-- need to flip (++) to et the right ordering (i.e. (+) is commutative, (++) isn't)
--      (++)   Fn-1 + Fn-2
-- flip (++)   Fn-2 + Fn-1
fibWords :: [a] -> [a] -> [[a]]
fibWords a b = fix ((a:) . (scanl (flip (++)) b))
