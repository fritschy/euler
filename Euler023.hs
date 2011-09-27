module Euler023 where
import Utility
import Data.Set hiding (filter, null)
euler023 = putNum . sum $ filter findSum [1..28123]
abundants' = filter ((== Abundant) . classify) [1..28122]
abundants  = fromList abundants'
findSum r  = null [a | a <- abundants', (r - a) `member` abundants]
