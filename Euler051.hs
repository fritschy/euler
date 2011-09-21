module Euler051 where

import Utility
import PrimeNumbers
import Data.List
import Data.Char

euler051 = putNum . head . snd . head . snd . head $ filter (not . null . snd) work

work = map replaceSome $ dropWhile (<56003) primes

replaceSome p = (p, filter ((==8) . length . snd) . map (\x->(x, anyValid p x)) $ concat [cl mlen | mlen<-[1..lsp - 1]])
                where sp = show p
                      cl mlen = filter ((==mlen - 1) . length) $ subsequences [0..mlen]
                      lsp = l10 p

anyValid p cs = filter (\x->l10 x == l10 p && isPrime x) $ map rcs [0..9]
                where rcs i = read $ map (\(x, e)->if x `elem` cs then intToDigit i else e) sp
                      sp = zip [0..] $ show p
