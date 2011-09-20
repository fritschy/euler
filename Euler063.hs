module Euler063 where
import Utility
euler063 = putNum . length $ filter (id) [e == (length $ show (n^e)) | n<-[1..10], e<-[1..100]]
