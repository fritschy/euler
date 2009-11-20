module Euler036 where
import Palindrome
import Utility
euler036 = sum [x | x <- [1..999999]::[Int], (isNumberPalindrome x) && (isPalindrome $ showBinary x)]
