module Utility (
  digitsOfNumber,       -- Integral a => a -> [Int]
  uniq,                 -- Eq a => [a] -> [a]
  sortUniq,             -- Ord a => [a] -> [a]
  sumUpTo,              -- Integral a => a -> a
  showBinary,           -- Integral a => a -> String
  showLiteralNumber,    -- Integral a => a -> String
  divisors,             -- Integral a => a -> [a]
  properDivisors,       -- Integral a => a -> [a]
  classify,             -- Integral a => a -> IntegralClass
  putNum,               -- Num a => a -> IO ()
  getWords,             -- String -> IO [String]
  wordSum,              -- (String -> Int)
  notSolved,            -- IO ()
  notRunnable,          -- IO ()
  l10,                  -- Integral a => a -> Int
  divides,              -- Integral a => a -> a -> Bool
  isPandigital,         -- Integral a => a -> Bool
  over,                 -- Integral a => a -> a -> a
  (!!=),                -- [a] -> (Int, a) -> [a]
  IntegralClass(..)
  ) where

import Data.Char
import Data.List
import Array
import qualified Data.Set as Set

over n r = f n `div` (f r * f (n - r))
           where f x = if x > 0 then x * f (x-1) else 1

-- Update list item at index p with e
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) (x:xs) (p, e) = if p == 0 then e:xs else x:(xs !!= (p-1, e))
(!!=) _      _      = []

l10 :: Integral a => a -> Int
l10 x = l x 0 where l x a = if x > 0 then l (x `div` 10) (a + 1) else a

divides :: Integral a => a -> a -> Bool
divides n d = d `mod` n == 0

isPandigital :: Integral a => a -> Bool
isPandigital = (\x->[1..length x] == (map digitToInt $ sort x)) . show

notSolved, notRunnable :: IO ()
notSolved = putStr "This problem is not solved yet."
notRunnable = putStr "This problem solution is not runnable."

wordSum :: String -> Int
wordSum = sum . map ((1 +) . ((- ord 'A') +) . ord)

getWords :: String -> IO [String]
getWords file = do words' <- readFile file
                   return . map (init . tail) $ splitWords words'
                   where splitWords [] = []
                         splitWords xs = word : splitWords (drop (1 + length word) xs)
                                         where word = takeWhile (/= ',') xs

putNum :: Num a => a -> IO ()
putNum = putStr . show

data IntegralClass = Deficient | Perfect | Abundant
     deriving (Eq)

classify :: Integral a => a -> IntegralClass
classify n = clasS (sum $ properDivisors n) n
             where clasS s n
                     | s == n = Perfect
                     | s <  n = Deficient
                     | s >  n = Abundant

----- proper divisors of n
---divisors :: Integral a => a -> [a]
---divisors2 n
---  | n < 1     = error $ "Invalid argument to divisors2: " ++ show n
---  | otherwise = divs (Set.fromList [1]) n 2
---
----- http://www.math.mtu.edu/mathlab/COURSES/holt/dnt/divis2.html
-----
----- One way to improve upon the above procedure is by applying the
----- following observation: if m is a divisor of n, then k = n/m is also a
----- divisor of n, because mk = n. Thus, the positive divisors can be organized
----- into pairs of the form (m, n/m), where m < n/m. The one exception to
----- this is if n is a perfect square and m = sqrt(n), in which case m =
----- n/m. For example, if n = 100, then the positive divisors of n are given by
---divs d n m
---  | m*m > n          = Set.toList d
---  | n `mod` m == 0   = divs (Set.insert m (if m*m == n then d else Set.insert (n`div`m) d)) n (m+1)
---  | otherwise        = divs d n (m+1)

divisors, properDivisors :: Integral a => a -> [a]
properDivisors = init . divisors
divisors n = (fst pairs) ++ (sort . wosqr $ snd pairs)
             where pairs = unzip pairs'
                   wosqr = if (last $ fst pairs) == (last $ snd pairs)
                             then init
                             else id
                   pairs' = [(m, dim) |
                              (m,(dim,mom)) <- zip [1..] $ map (divMod n)
                                [1..truncate(sqrt(fromIntegral n))], 0==mom]

digitsOfNumber :: Integral a => a -> [Int]
digitsOfNumber = map digitToInt . show

-- remove adjacent duplicate elements, i.e. only usefull when zthe
-- list is sorted beforehand
uniq :: Eq a => [a] -> [a]
uniq (x:xs@(y:_)) = if x == y then uniq xs else x : uniq xs
uniq (x:[]) = [x]
uniq [] = []

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
