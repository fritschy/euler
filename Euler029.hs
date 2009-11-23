module Euler029 where
import Utility
-- faster than nub!
euler029 = putNum . length $ sortUniq [a^b | a <- [2..100], b <- [2..100]]
