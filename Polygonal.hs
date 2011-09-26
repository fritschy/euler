module Polygonal where

polygonal :: Integral a => Int -> a -> a
polygonal 3 n = n * (n + 1)     `div` 2
polygonal 4 n = n *  n
polygonal 5 n = n * (3 * n - 1) `div` 2
polygonal 6 n = n * (2 * n - 1)
polygonal 7 n = n * (5 * n - 3) `div` 2
polygonal 8 n = n * (3 * n - 2)
polygonal _ _ = undefined

polygonals :: Integral a => Int -> [a]
polygonals d = map (polygonal d) [1..]

triangle, square, pentagonal, hexagonal, heptagonal, octagonal :: Integral a => a -> a
triangle   = polygonal 3
square     = polygonal 4
pentagonal = polygonal 5
hexagonal  = polygonal 6
heptagonal = polygonal 7
octagonal  = polygonal 8

triangles, squares, pentagonals, hexagonals, heptagonals, octagonals :: Integral a => [a]
triangles   = map triangle   [1..]
squares     = map square     [1..]
pentagonals = map pentagonal [1..]
hexagonals  = map hexagonal  [1..]
heptagonals = map heptagonal [1..]
octagonals  = map octagonal  [1..]
