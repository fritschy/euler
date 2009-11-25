module Euler005 where
import Utility
euler005 = putNum $ foldl1 lcm [2..20]
