module Euler042 where
import Utility
euler042 = do words'' <- getWords "data/words.txt"
              let words' = words''
                  words = map wordSum words'
                  trinums = takeWhile (< (maximum words)) [n * (n+1) `div` 2 | n <- [1..]]
              putNum . length $ filter (`elem` trinums) words
