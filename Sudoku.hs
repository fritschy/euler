module Sudoku where

import Data.List
import Data.Char

type Block = [Int]

data Sudoku = Sudoku { rows :: [Block] } deriving (Eq)

instance Show Sudoku where
  show = unlines . map (map intToDigit) . rows

readSudoku :: String -> Sudoku
readSudoku str = if not (isSudoku s) || not (valid s)
                   then error "Not a sudoku!"
                   else s
                 where wrap x = if x `elem` ".- " then 0 else digitToInt x
                       s      = Sudoku . map (map wrap) $ lines str

fromList :: [[Int]] -> Sudoku
fromList = Sudoku

toList :: Sudoku -> [[Int]]
toList = rows

isSudoku :: Sudoku -> Bool
isSudoku = all ((== 9) . length) . rows

isSolved :: Sudoku -> Bool
isSolved = all (all (/= 0)) . rows

blockValid :: Block -> Bool
blockValid = noDoubles . sort . filter (/= 0)
             where noDoubles (x:y:xs) = if x /= y then noDoubles (y:xs) else False
                   noDoubles _        = True

blocks :: Sudoku -> ([Block], [Block], [Block])
blocks s = (rs, transpose rs, [b r c 0 0 | r <- ots, c <- ots])
           where b _ _ 3 _ = []
                 b r c y 3 = b r c (y+1) 0
                 b r c y x = (rs!!(r+y)!!(c+x)) : b r c y (x+1)
                 rs        = rows s
                 ots       = [0,3,6]

valid :: Sudoku -> Bool
valid = allValid . blocks
        where allValid (r, c, b) = v r && v c && v b
              v                  = and . map blockValid

type Pos = (Int, Int)

blanks :: Sudoku -> [Pos]
blanks = b 0 . concat . rows
         where b c (0:xs) = divMod c 9 : b (c+1) xs
               b c (_:xs) = b (c+1) xs
               b c _      = []

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) = flip u
        where u (i, e) = map (\(y,x) -> if y == i then e else x) . zip [0..]

update :: Sudoku -> Pos -> Int -> Sudoku
update s (r, c) e = Sudoku . map (\(y,x)->if y == r then x !!= (c, e) else x) . zip [0..] $ rows s

candidates :: Sudoku -> Pos -> [Int]
candidates = flip cand
             where cand (r, c) s = if rows s!!r!!c /= 0 then []
                                     else [1..9] \\ (rs ++ cs ++ bs)
                                   where (br, bc, bb) = blocks s
                                         rs = br !! r
                                         cs = bc !! c
                                         bs = bb !! ((r `div` 3) * 3 + c `div` 3)

eliminateSingles s = if null singles then s
                     else es s singles
                     where singles = filter l1 $ map (\(x,y)->(y, x, candidates s (y, x))) coords 
                           l1 (_,_,(_:[]))       = True
                           l1 _                  = False
                           es s ((r, c, [e]):xs) = es (update s (r, c) e) xs
                           es s _                = s
                           coords                = [(x, y)|y<-[0..8], x<-[0..8]]

apply :: Int -> (a -> a) -> a -> a
apply 0 f s = s
apply n f s = apply (n-1) f $ f s

solve :: Sudoku -> [Sudoku]
solve s = if not (isSudoku s) || not (valid s)
            then []
            else backtrack (apply 5 eliminateSingles s) []

backtrack :: Sudoku -> [Sudoku] -> [Sudoku]
backtrack s acc = if null bs
                    then s:acc
                    else concat sols
                  where bs   = blanks s
                        hbs  = head bs
                        cs   = candidates s hbs
                        sols = filter (not . null) $ map (\x->backtrack (update s hbs x) acc) cs
