module Euler096 where

import Utility
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

import Control.Parallel

import Sudoku

euler096 = do buf <- readFile "data/sudoku.txt"
              -- get lines of buffer
              -- make sudoku lists
              -- map
              --   make Sudoku from lists
              --   solve sudokus
              --   unwrap Just [Sudoku]
              --   take the head off
              --   make it into an [[Int]]
              --   take the first row
              --   take the first 3 digits
              --   make an int
              -- sum
              putNum . sum . map (mkInt . take 3 . head . toList . head . fromJust . solve . fromList) . mkSudokus $ lines buf
              where mkInt (x:y:z:[]) = x*100 + y*10 + z

mkSudokus x
  | 9 > length x  = []
  | otherwise     = map (map digitToInt) a : mkSudokus b
                    where (_:a, b) = splitAt 10 x
