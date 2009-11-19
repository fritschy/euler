module Euler029 where
import Data.List
import Utility
-- faster than nub!
euler029 = length . uniq $ sort [a^b | a <- [2..100], b <- [2..100]]
