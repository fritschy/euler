module Euler005 where
import Utility
euler005 = putNum $ foldl lcm 1 [2..20]
