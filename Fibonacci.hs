module Fibonacci(
  fibs0,
  fibs,
  fibWords
  ) where

import Control.Monad.Fix -- works with hugs and ghc (hugs doesn't know Data.Function)

fibs_old0, fibs_old :: Integral a => [a]
fibs_old0 = fix ((0:) . (scanl (+) 1))
fibs_old  = tail $ fibs0

-- need to flip (++) to et the right ordering (i.e. (+) is commutative, (++) isn't)
--      (++)   Fn-1 + Fn-2
-- flip (++)   Fn-2 + Fn-1
fibWords :: [a] -> [a] -> [[a]]
fibWords a b = fix ((a:) . (scanl (flip (++)) b))

fibs, fibs0 :: Integral a => [a]
fibs = f 0 1 where f a b = b:f b (a + b)
fibs0 = 0:fibs
