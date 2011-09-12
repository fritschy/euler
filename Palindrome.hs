module Palindrome (isShowPalindrome, isPalindrome) where

isShowPalindrome :: Show a => a -> Bool
isShowPalindrome = isPalindrome . show

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
