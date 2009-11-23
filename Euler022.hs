module Euler022 where

import System.IO

getNames = do names' <- readFile "data/names.txt"
              putStrLn . unlines $ lines names'
              return (lines names')

euler022 = False

