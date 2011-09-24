module Euler054 where

import Utility
import Data.List
import System.IO
import Control.Monad
import Data.Maybe
import Data.Char

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Show, Eq)

type CardVal = Int

data Card = Card { value :: CardVal
                 , suit  :: Suit } deriving (Show, Eq)

instance Ord Card where
  compare a b = value a `compare` value b

type Cards = [Card]

toCardVal :: Char -> Int
toCardVal = fromJust . flip lookup (zip "23456789TJQKA" [2..])

toSuit :: Char -> Suit
toSuit 'S' = Spades
toSuit 'H' = Hearts
toSuit 'D' = Diamonds
toSuit 'C' = Clubs

toCard :: String -> Card
toCard (v:s:_) = Card (toCardVal v) (toSuit s)
toCard _       = undefined

data Hand = Hand { rank  :: Rank
                 , cards :: Cards
                 } deriving (Show, Eq)

instance Ord Hand where
  compare = compareHands

toHand :: Cards -> Hand
toHand cs = Hand (toRank scs) (reverse scs) where scs = sort cs

data Rank = HighCard
          | OnePair CardVal
          | TwoPairs (CardVal,CardVal)
          | ThreeOfAKind CardVal
          | Straight CardVal -- Straight beginning at CardVal
          | Flush Suit
          | FullHouse (CardVal,CardVal) -- 3ofAKind+Pair
          | FourOfAKind CardVal
          | StraightFlush CardVal
          | RoyalFlush
          deriving (Show, Eq)

instance Ord Rank where
  compare = compareRank

compareRank :: Rank -> Rank -> Ordering

compareRank RoyalFlush _                        = GT
compareRank _ RoyalFlush                        = LT

compareRank (StraightFlush a) (StraightFlush b) = compare a b
compareRank (StraightFlush _) _                 = GT
compareRank _ (StraightFlush _)                 = LT

compareRank (FourOfAKind a) (FourOfAKind b)     = compare a b
compareRank (FourOfAKind _) _                   = GT
compareRank _ (FourOfAKind _)                   = LT

compareRank (FullHouse a) (FullHouse b)         = compare a b
compareRank (FullHouse _) _                     = GT
compareRank _ (FullHouse _)                     = LT

compareRank (Flush _) _                         = GT
compareRank _ (Flush _)                         = LT

compareRank (Straight a) (Straight b)           = compare a b
compareRank (Straight _) _                      = GT
compareRank _ (Straight _)                      = LT

compareRank (ThreeOfAKind a) (ThreeOfAKind b)   = compare a b
compareRank (ThreeOfAKind _) _                  = GT
compareRank _ (ThreeOfAKind _)                  = LT

compareRank (TwoPairs a) (TwoPairs b)           = compare a b
compareRank (TwoPairs _) _                      = GT
compareRank _ (TwoPairs _)                      = LT

compareRank (OnePair a) (OnePair b)             = compare a b
compareRank (OnePair _) _                       = GT
compareRank _ (OnePair _)                       = LT

compareRank _ _                                 = EQ

areSuccessive :: Cards -> Bool
areSuccessive (Card x _:n@(Card y _):xs) = if x+1 == y then areSuccessive (n:xs) else False
areSuccessive (x:_)                      = True

sameSuit :: Cards -> Bool
sameSuit (Card _ x:n@(Card _ y):xs) = if x == y then sameSuit (n:xs) else False
sameSuit (x:_)                      = True

compareHands :: Hand -> Hand -> Ordering
compareHands (Hand ra ca) (Hand rb cb) = case compare ra rb of
                                           EQ -> compare ca cb
                                           x  -> x

toRank :: Cards -> Rank
toRank h = fromJust . head . dropWhile isNothing $ map (\f->f h) fs
           where fs = [ isRoyalFlush
                      , isStraightFlush
                      , isFourOfAKind
                      , isFullHouse
                      , isFlush
                      , isStraight
                      , isThreeOfAKind
                      , isTwoPairs
                      , isOnePair
                      , (\_->Just HighCard) ]

isOnePair, isTwoPairs, isThreeOfAKind, isStraight, isFlush, isFullHouse,
  isFourOfAKind, isStraightFlush, isRoyalFlush :: Cards -> Maybe Rank

isOnePair h = i h
  where i (Card x _:n@(Card y _):xs) =
          if x == y
            then Just $ OnePair x
            else i (n:xs)
        i (x:_) = Nothing

isTwoPairs h = i [] h
  where i a (Card x _:n@(Card y _):xs) =
          if x == y then i (x:a) xs else i a (n:xs)
        i (x:y:_) _ =
          if x /= y
            then Just $ TwoPairs (x,y)
            else Nothing
        i a _ = Nothing

isThreeOfAKind h = i h
  where i (Card x _:n@(Card y _):m@(Card z _):xs) =
          if x == y && y == z
            then Just $ ThreeOfAKind x
            else i (n:m:xs)
        i (x:_) = Nothing

isStraight h = if areSuccessive h
                 then Just (Straight (value $ head h))
                 else Nothing

isFlush h = if sameSuit h
              then Just $ Flush (suit $ head h)
              else Nothing

isFullHouse h = case h of
  (Card a _:Card b _:Card c _:Card d _:Card e _:_) ->
    if      a == b && b == c && d == e then Just $ FullHouse (a, d)
    else if a == b && c == d && d == e then Just $ FullHouse (c, a)
    else Nothing
  _ -> Nothing

isFourOfAKind h = case h of
  (Card a _:Card b _:Card c _:Card d _:Card e _:_) ->
    if      a == b && b == c && c == d then Just $ FourOfAKind a
    else if b == c && c == d && d == e then Just $ FourOfAKind b
    else Nothing
  _ -> Nothing

isStraightFlush h = if areSuccessive h && sameSuit h
                      then Just (StraightFlush $ value hc)
                      else Nothing
                    where hc = head h

isRoyalFlush h = if areSuccessive h && sameSuit h && value hc == 10
                   then Just RoyalFlush
                   else Nothing
                 where hc = head h

euler054 = do hands <- makeHands `liftM` readFile "data/poker.txt"
              putNum . length $ filter (\(a,b)->a > b) hands

makeHands :: String -> [(Hand, Hand)]
makeHands = map makeHand . lines

makeHand :: String -> (Hand, Hand)
makeHand = (\(a,b)->(toHand a, toHand b)) . splitAt 5 . map toCard . words
