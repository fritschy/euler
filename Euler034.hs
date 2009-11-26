module Euler034 where
import Utility
import Data.Char
import Data.Array
euler034 = putNum . sum . map snd . filter (\x->(snd x) == (fst x)) . zip [3..] $ map (sum . map ((f!) . digitToInt) . show) [3..50000]
           where f = array (0,9) $ zip [0..] (scanl (*) 1 [1..9])
