module Utility (
  digitsOfNumber,                            -- Integral a => a -> [Int]
  uniq,                                      -- Eq a => [a] -> [a]
  sortUniq,                                  -- Ord a => [a] -> [a]
  sumUpTo,                                   -- Integral a => a -> a
  showBinary,                                -- Integral a => a -> String
  showLiteralNumber,                         -- Integral a => a -> String
  divisors,                                  -- Integral a => a -> [a]
  classify,                                  -- Integral a => a -> IntegralClass
  putNum,                                    -- Num a => a -> IO ()
  IntegralClass(..)
  ) where

import Data.Char
import Data.List
import Array

putNum :: Num a => a -> IO ()
putNum = putStr . show

data IntegralClass = Deficient | Perfect | Abundant
     deriving (Eq)

classify :: Integral a => a -> IntegralClass
classify n = clasS (sum $ divisors n) n
             where clasS s n
                     | s == n = Perfect
                     | s <  n = Deficient
                     | s >  n = Abundant

-- proper divisors of n
divisors :: Integral a => a -> [a]
divisors n
  | n <  0    = error $ "Invalid argument to divisors: " ++ show n
  | otherwise = 1 : divs n 2
                where divs n d
                        | n `mod` d == 0 = d : divs n (d+1)
                        | d > n `div` 2  = []
                        | otherwise      = divs n (d+1)

digitsOfNumber :: Integral a => a -> [Int]
digitsOfNumber n
  | n >= 0       = map (sub0 . ord) $ show n
  | otherwise    = error $ "Invalid argument to digitsOfNumber: " ++ show n
                   where sub0 n = n - 0x30

-- remove adjacent duplicate elements, i.e. only usefull when zthe
-- list is sorted beforehand
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs)
  | not (null xs) = if x == head xs then uniq xs else x : uniq xs
  | otherwise     = [x]

sortUniq :: Ord a => [a] -> [a]
sortUniq = uniq . sort

-- sum the numbers from 1..n
sumUpTo :: Integral a => a -> a
sumUpTo n = n * (n + 1) `div` 2
--sumUpTo n = n + n * (n - 1) `div` 2

showBinary :: Integral a => a -> String
showBinary x
  | x > 0     = (if x `mod` 2 == 1 then '1' else '0') : showBinary (x `div` 2)
  | otherwise = []

oneToNine :: Int -> String
oneToNine n
  | n >= 1 && n <= 9 = chars!(n-1)
  | otherwise        = error $ "Number not in range 1..9 given to oneToNine: " ++ show n
                       where chars = array (0,8) [(0, "one"), (1, "two"), (2, "three"), (3, "four"), (4, "five"),
                                                  (5, "six"), (6, "seven"), (7, "eight"), (8, "nine")]

oneToNine10 :: Int -> String
oneToNine10 n
  | n >= 2 && n <= 9 = chars!(n-1)
  | otherwise        = error $ "Number not in range 2..9 given to oneToNine10: " ++ show n
                       where chars = array (0,8) [(0, "one"), (1, "twenty"), (2, "thirty"), (3, "forty"), (4, "fifty"),
                                                  (5, "sixty"), (6, "seventy"), (7, "eighty"), (8, "ninety")]

teens :: Int -> String
teens n
  | n >= 10 && n <= 19 = chars!(n-10)
  | otherwise          = error $ "Number not in range 1..9 given to teens: " ++ show n
                         where chars = array (0,9) [(0, "ten"), (1, "eleven"), (2, "twelve"), (3, "thirteen"), (4, "fourteen"),
                                                    (5, "fifteen"), (6, "sixteen"), (7, "seventeen"), (8, "eighteen"), (9, "nineteen")]

showLiteralNumber :: Integral a => a -> String
showLiteralNumber n
  | n == 0                 = []
  | n >= 1000 && n <= 9999 = ((oneToNine (head digits)) ++ " thousand ") ++ showLiteralNumber (next 1)
  | n >=  100 && n <=  999 = ((oneToNine (head digits)) ++ " hundred " ++ (if (next 1) == 0 then [] else "and ")) ++ showLiteralNumber (next 1)
  | n >=   20 && n <=   99 = (oneToNine10 (head digits)) ++ showLiteralNumber (next 1)
  | n >=   10 && n <=   19 = (teens ((head digits)*10 + (head $ tail digits))) ++ showLiteralNumber (next 2)
  | n >=    1 && n <=    9 = (oneToNine (head digits)) ++ showLiteralNumber (next 1)
                             where digits = digitsOfNumber n
                                   next x = read . ('0':) . concat . map show . (if x == 2 then tail else id) $ tail digits
