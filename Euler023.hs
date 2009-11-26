module Euler023 where
import Utility
import Data.Set hiding (filter, null)
euler023 = notSolved --putStr . show $ filter findSum [1..20161]
           where abundants' = takeWhile (<= 20162) $ filter ((== Abundant) . classify) numbers
                 abundants  = fromList abundants'
                 numbers    = [x | x <- [2..20161]]
                 findSum r  = null [a | a <- abundants', (r - a) `member` abundants]
