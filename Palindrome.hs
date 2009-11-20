module Palindrome (isNumberPalindrome, isPalindrome) where

isNumberPalindrome :: Integral a => a -> Bool
isNumberPalindrome n = isPalindrome $ show n

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
