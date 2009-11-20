module Euler017 where

import Utility
import Array
import Data.Char

euler017 = length $ concat [concat . words $ showLiteralNumber n | n <- [1..1000]]
--euler017 = [(n, showLiteralNumber n) | n <- [1..9999]]
