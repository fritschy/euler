module Fibonacci where
import Control.Monad.Fix -- works with hugs and ghc (hugs doesn't know Data.Function)
fibs0 = fix ((0:) . (scanl (+) 1))
fibs  = tail $ fibs0
