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
> next_permutation = cat . rev . swap . split . ini . check
> check [] = error "next_permutation: [] invalid"
> check a = reverse a
> ini a = (tail a, [head a])
> split (a, b)
>   | null a || head a < head b = (a, b)
>   | otherwise                 = split (tail a, head a : b)
> swap (a, b) = swap2 (a, b, (length b)-1)
>   where swap2 (a, b, m)
>           | head a < bm = (bm : tail a,
>                             (fst sb) ++
>                               head a :
>                                 (tail $ snd sb))
>           | otherwise   = swap2 (a, b, m-1)
>           where sb = splitAt m b
>                 bm = b!!m
> rev (a, b) = (b, a)
> cat (a, b) = reverse $ a ++ b

Implementation notes:

The code is based on the description above, however, as lists can be inefficient
When handled poorly, the sequence is reversed before we start processing.
Only the final step does restire the correct order - note too, that function rev
only switches a and b, such that they can be concatenated as "expected".

Still, that monster takes >80 seconds for the 1e6 permutation of a 10-len
sequence - which is too long.

Some test:
let p = permutations in and $ map (\x->x == sort x) [p [1..x] | x <- [1..6]]

> permutations a = take (product [2..(length a)]) $ iterate' next_permutation a
>                  where iterate' f x = x : (iterate' f $! f x)
