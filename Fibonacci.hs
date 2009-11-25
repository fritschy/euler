module Fibonacci where
import Control.Monad.Fix -- works with hugs and ghc (hugs doesn't know Data.Function)
fibs0 = fix ((0:) . (scanl (+) 1))
fibs  = tail $ fibs0

-- need to flip (++) to et the right ordering (i.e. (+) is commutative, (++) isn't)
fibWords a b = fix ((a:) . (scanl (flip (++)) b))
