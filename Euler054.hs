module Euler054 where

import Utility
import Data.List
import System.IO
import Control.Monad
import Data.Maybe
import Data.Char

data Suit = Sp | He | Di | Cl deriving (Show, Eq)

type CardVal = Int

data Card = Card { value :: CardVal
                 , suit  :: Suit } deriving (Show, Eq)

instance Ord Card where
  compare a b = value a `compare` value b

toCardVal :: Char -> Int
toCardVal = fromJust . flip lookup (zip "23456789TJQKA" [2..])

toSuit :: Char -> Suit
toSuit 'S' = Sp
toSuit 'H' = He
toSuit 'D' = Di
toSuit 'C' = Cl

toCard :: String -> Card
toCard (v:s:_) = Card (toCardVal v) (toSuit s)

data Hand = Hand { cards :: [Card] } deriving (Show, Eq)

instance Ord Hand where
  compare = compareHands

compareHands :: Hand -> Hand -> Ordering
compareHands = undefined

-- XXX need to take into account highest card for tie and also the value of the pairs!
points :: Hand -> Int
points h = if      isRoyalFlush h    then 1000
           else if isStraightFlush h then  999
           else if is4OfAKind h      then  998
           else if isFullHouse h     then  997
           else if isFlush h         then  996
           else if isStraight h      then  995
           else if is3OfAKind h      then  994
           else if is2Pairs h        then  993
           else if isPair h          then  991
           else    value . head . sortBy (flip compare) $ cards h

-- all :: Hand -> Bool
isRoyalFlush h = undefined
isStraightFlush h = undefined
is4OfAKind h = undefined
isFullHouse h = undefined
isFlush h = undefined
isStraight h = undefined
is3OfAKind h = undefined
is2Pairs h = undefined
isPair h = undefined

euler054 = withFile "data/poker.txt" ReadMode $ \h -> do
             hands <- map ((\(a,b)->(Hand a, Hand b)) . splitAt 5 . map toCard . words) `liftM` lines `liftM` hGetContents h
             putStr $ show $ head hands
