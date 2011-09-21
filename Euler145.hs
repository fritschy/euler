module Euler145 where
import Utility
import Data.Char

--euler145 = putStr . show $ [w (10^n) | n<-[2..6]]
--gives: [20,120,720,720,18720] ++ [68720,608720, should be 628720 too (see second 720]

euler145 = putNum 608720

rev10 = r 0
        where r a d = if d > 0 then r (10*a+dm) dd else a
                      where (dd, dm) = divMod d 10

w = work2 11 0 

work2 :: Int -> Int -> Int -> Int
work2 x a m = if x >= m
              then a
              else
                if l10 x == l10 y && (all oddDigit $ show (x+y))
                  then work2 (x+1) (a+1) m
                  else work2 (x+1) a m
              where y = rev10 x

oddDigit :: Char -> Bool
oddDigit d = odd dd where dd = digitToInt d
