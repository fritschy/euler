module Euler052 where
import Data.List
euler052 = putStr . fst . head $ filter (and . snd) [(show x, zipWith (==) (replicate 5 (ss x)) [ss (x*n)|n<-[2..6]])|x<-[100000..]]
           where ss = sort . show
