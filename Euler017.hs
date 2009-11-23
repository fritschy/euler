module Euler017 where
import Utility
euler017 = putNum . length $ concat [concat . words $ showLiteralNumber n | n <- [1..1000]]
