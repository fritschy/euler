module Euler023 where
import Utility
import Data.Set hiding (filter, null)
-- actually the smallest number from which on every number can be
-- formed by adding to abundant number is 20162, which is documented
-- on wikipedia.Nevertheless I am using the hiugher bound to check
-- my program for correctness.
euler023 = putNum . sum $ filter findSum [1..28123]
abundants' = takeWhile (< 28123) $ filter ((== Abundant) . classify) [1..28123]
abundants  = fromList abundants'
findSum r  = null [a | a <- abundants', (r - a) `member` abundants]
