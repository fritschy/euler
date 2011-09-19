module Euler096 where

import Utility
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

import Control.Parallel.Strategies

import Sudoku

euler096 :: IO()
euler096 = do buf <- readFile "data/sudoku.txt"
              putNum . sum . parMap rwhnf (mkInt . head . toList . head . solve . fromList) . mkSudokus $ lines buf
              where mkInt (x:y:z:_) = x*100 + y*10 + z

mkSudokus x
  | 9 > length x  = []
  | otherwise     = map (map digitToInt) a : mkSudokus b
                    where (_:a, b) = splitAt 10 x
