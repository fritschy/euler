module Euler024 where
import Utility
import Permutation
import Data.Char
euler024 = putStr . map intToDigit $ (permutations [0..9])!!999999
