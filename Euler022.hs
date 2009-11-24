module Euler022 where

import Data.Char
import Utility
import Data.List

euler022 = do names' <- readFile "data/names.txt"
              let names = zip [1..] . sort . map (init . tail) $ splitNames names'
              putNum . sum $ map val names
              where val (i, n) = i * (sum $ map ((1 +) . ((- ord 'A') +) . ord) n)
                    splitNames [] = []
                    splitNames xs = name : splitNames (drop (1 + length name) xs)
                                    where name = takeWhile (/= ',') xs
