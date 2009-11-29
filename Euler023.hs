module Euler023 where
import Utility
import Data.Set hiding (filter, null)
euler023 = putStr . show $ filter findSum [1..20161]
abundants' = takeWhile (<= 20162) $ filter ((== Abundant) . classify) [1..20161]
abundants  = fromList abundants'
findSum r  = null [a | a <- abundants', (r - a) `member` abundants]
