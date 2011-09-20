module Euler051 where

import Utility
import PrimeNumbers
import Data.List
import Data.Char

--euler051 = putStr . show . take 100 . filter (\(p,c,x)->length x == 8) . foldr (++) [] $ foldr (++) [] work

for = flip map

work = map replaceSome $ filter (>90000) primes

--filters = sortUniq . filter ((==length sp) . length . show . (id::Integer->Integer) . read)

--replaceSome p = [[(p, xcl, filter (\x->isPrime x && l10 x == lsp) [read $ map (\(p,c)->if p `elem` xcl then intToDigit n else c) $ zip [0..] sp | n<-[0..9]]) | xcl<-cl mlen] | mlen<-[1..length sp - 1]]
replaceSome p = (p, [xcl | mlen<-[1..lsp - 1], xcl<-cl mlen]) -- replcae-index-list for each prime
                where sp = show p
                      cl mlen = sortBy (flip compare) . sortUniq . map (sort . drop 1) $ permutations [0..mlen] -- select uniq replace-index-list
                      lsp = l10 p
