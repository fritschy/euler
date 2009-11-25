module Euler022 where

import Utility
import Data.List

euler022 = do names' <- getWords "data/names.txt"
              let names = zip [1..] . sort $ names'
              putNum . sum $ map val names
              where val (i, n) = i * (wordSum n)
