module Utility where
import Data.Char
digitsOfNumber n = map (sub0 . ord) $ show n
                   where sub0 n = n - 0x30
