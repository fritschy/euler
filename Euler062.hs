module Euler062 where
import Utility
import Data.List
import qualified Data.Map as M
euler062 = putNum . head . head . map (\(_,a)->sort a) . filter ((==5) . length . snd) . M.toList $ foldl' (\acc (k,v) -> M.insertWith (++) k v acc) M.empty (map ((\x->(sort $ show x, [x])) . (^3)) [1000..9999])
