> module Permutation(next_permutation, permutations) where

As I don't have a copy of TAOCP, this description is from
http://blog.bjrn.se/2008/04/lexicographic-permutations-using.html

Algorithm L: Given a sequence of n elements a0, a1, …, an-1 generate all
permutations of the sequence in lexicographically correct order.

Step 1 (Step L2 in Knuth): Partition the sequence into two sequences a0, a1,
…, aj and aj+1, aj+2, …, an-1 such that we have already generated all
permutations beginning with a0, a1, …, aj. This can by done by decreasing
j from n-2 until aj < aj+1. If j = 0 we are done.

For example, the input sequence 1, 4, 3, 2 is split into the sequence 1 and
the sequence 4, 3, 2. Obviously, there are no more lexicographic permutations
beginning with 1 when the second sequence is in decreasing order.

Step 2a (Step L3 in Knuth): In the second sequence aj+1, aj+2, …, an-1
working backwards, find am, the first value larger than aj. We find am by
setting m to n-1 and decreasing until aj < am.

Step 2b: Swap aj and am.

For example, our two sequences are 1 and 4, 3, 2. As the second sequence is
decreasing because of the first step, am is the smallest element greater than
aj that can legitimately follow a0, a1, …, aj-1 in a permutation.

Step 3 (Step L4 in Knuth): Reverse aj+1, aj+2, …, an-1.

Here are some examples of the steps on (1, 2, 3, 4) that should clarify step
2a, 2b and 3:

(1, 2, 3, 4) >> (1, 2, 3), (4) >> (1, 2, 4), (3) >> (1, 2, 4), (3) >> (1, 2, 4, 3)
(1, 2, 4, 3) >> (1, 2), (4, 3) >> (1, 3), (4, 2) >> (1, 3), (2, 4) >> (1, 3, 2, 4)
(1, 3, 2, 4) >> (1, 3, 2), (4) >> (1, 3, 4), (2) >> (1, 3, 4), (2) >> (1, 3, 4, 2)
(1, 3, 4, 2) >> (1, 3), (4, 2) >> (1, 4), (3, 2) >> (1, 4), (2, 3) >> (1, 4, 2, 3)

Here are some examples on the sequence (1, 2, 2, 3):

(1, 2, 2, 3) >> (1, 2, 2), (3) >> (1, 2, 3), (2) >> (1, 2, 3), (2) >> (1, 2, 3, 2)
(1, 2, 3, 2) >> (1, 2), (3, 2) >> (1, 3), (2, 2) >> (1, 3), (2, 2) >> (1, 3, 2, 2)
(1, 3, 2, 2) >> (1), (3, 2, 2) >> (2), (3, 2, 1) >> (2), (1, 2, 3) >> (2, 1, 2, 3)

Algorithm L is a fairly simple algorithm and it's also easy to understand and
implement. Permutation generation is sometimes used as an interview question
because it's difficult to get right even though the underlying problem is
easy to grasp. It can thus be useful to know even for those not interested
in combinatorics.

> next_permutation :: Ord a => [a] -> [a]
> next_permutation = check >.> ini >.> split >.> swap >.> rev >.> cat
>   where check [] = error "next_permutation: [] invalid"
>         check a = a
>         ini a = (init a, [last a])
>         split (a, b)
>           | null a || last a < head b = (a, b)
>           | otherwise                 = split (init a, last a : b)
>         swap (a, b)
>           | otherwise        = swap2 (a, b, (length b)-1)
>           where swap2 (a, b, m)
>                   | last a < b!!m   = (init a ++ [b!!m],
>                                         (fst $ splitAt m b) ++
>                                           last a :
>                                             (tail . snd $ splitAt m b))
>                   | otherwise       = swap2 (a, b, m-1)
>         rev (a, b) = (a, reverse b)
>         cat (a, b) = a ++ b
>         (>.>) = flip (.)

>-- let p = permutations in and $ map (\x->x == sort x) [p [1..x] | x <- [1..6]]

> permutations a = p a 0 $ product [2..(length a)]
>   where p a c n
>           | c == n    = []
>           | otherwise = a : p (next_permutation a) (c+1) n
