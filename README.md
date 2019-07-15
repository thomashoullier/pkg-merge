# Package-merge algorithm Common Lisp implementation
We implemented the package-merge algorithm [1] for solving the Coin Collector 
problem. This is mainly a (hopefully clear) write-up
of the details of the package-merge algorithm for the sake of documentation.

This is linked to the problem of length-limited Huffman coding, but we just
limit ourselves to the Coin Collector problem here for the sake of clarity.

⚠⚠⚠ The implementation is for the sake of testing and research. Better
algorithms have been published for solving the length-limited Huffman coding
problem with better space performance [2]. Please ponder a while before using
this in production as is. ⚠⚠⚠

## Problem presentation
[1] introduces the (binary) "Coin Collector's problem". A numismat has run out
of money and must pay for his groceries with his coin collection. Each coin has
a face value and a numismatic value. The numismat wants to find the set of coins
that have a given total face value while minimizing the total numismatic value.

The coins can only have face values in (possibly negative) powers of two. The
problem is then suited to finding sets of coins amounting to total sums _X_ that
are easily decomposed in a sum of powers of two.

Let us represent coins by the pair: (_i_, _w_).
* _i_: The coin has face value 2^i.
* _w_: Weight of the coin, its numismatic value.

We can state the problem more formally. We want to find, for a given set of
pairs (_i_, _w_), the subset whose sum of _2^i_ equals X while minimizing the
sum of _w_.

## Algorithm explanation
(The Binary Coin Collector can actually be solved in intuitive ways. You can
think about how you would proceed were you in the coin collector's shoes and
find a good procedure. Solution is below if you lack the patience :) )

Two slightly different ways of looking at the solving procedure are given in [1]
: a recursive and non-recursive one. We also give the intuitive idea behind the
algorithm, which could maybe elude the readers of [1].

### General intuitive idea
Let us take a particular problem defined by:
* The coins with face values _2^i_ and numismatic values _w_:

| | | | | | | | | |
| ---- | -- | -- | -- | -- | -- | -- | -- | -- |
| coin id | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
| face | -2 | -2 | -2 | -2 | -1 | -1 | 1 | 1 |
| weight | 1 | 1 | 3 | 4 | 2 | 5 | 1 | 3 |

* The sum _X_ to pay and its diadic expansion terms _k_. _X = Sum(2^k)_.

| -- | -- | -- | -- |
| k | -2 | 0 | 1 |

### Recursive point of view
Let us consider:
* A set _I_ of coins.
* A real number _X_. Decomposed in powers of two. This is the total face value
we want to match with a subset of _I_.

We will update _I_ and _X_ throughout the procedure.
* _r_ is the smallest _2^i_ of all the items in _I_. ie the smallest face value
available in _I_.
* _a_ is the coin with smallest numismatic value _w_ out of the coins of face 
value _r_.
* _minwidth_ is the smallest power of two in the decomposition of X.

We call the optimal subset of coins for _I_ and _X_: _S(I,X)_.

There are four cases:
1. _r > minwidth_: **No solution**. Our smallest coin in face value is too big
to cover the smallest power of two in _X_.
1. _r = minwidth_: _S(I,X) = S(I - {a}, X - r) U {a}_. In other words, we use
the coin to pay for the part _r_ of _X_. The solution set to our problem is
then, recursively, _{a}_ plus the solution set to the problem with _r_ paid for 
in _X_ and without the coin _a_ in _I_.
1. _r < minwidth_ and _a_ is the sole coin of value _r_: 
_S(I,X) = S(I - {a}, X)_. In other words, throw away coin _a_. Its face value is
too low to pay for _r_ and it cannot be combined with other coins. Solve the
problem without _{a}_ in _I_.
1. _r < minwidth_ and there are at least two items with value _r_ in _I_. Take
_a_ and _a'_ the two coins of value _r_ with smallest _w_. Form a **package**
_b_. The face value and numismatic value of _b_ are the sum of that of _a_ and
_a'_. The solution is then recursively _S(I - {a, a'} U {b}, X)_ (if we allow
_b_ to be part of the solution set and we know it is a package of {a, a'}).
Explained simply, we form a package with the two coins of smallest numismatic
value and include it in _I_, treating it as a new virtual coin, after removing 
the two original coins.

And terminations:
* If _X = 0_, then _S(I,X)_ is empty.
* If _X > 0_ and _I_ is empty then there is **no solution**.

### Non-recursive point of view
We arrange the set of coins I in subsets _Ld_ according to their face value
_2^d_. The sets _Ld_ are sorted by non-decreasing weights. Let _S_ be an optimal
solution set of the problem. _S_ is empty at the start of the algorithm.
We alter very slightly the formulation of [1] so as
to make things clearer (in our view).
The algorithm goes as follows:

* Loop while _X > 0_
  * _minwidth_ = smallest _2^d_ term in the diadic expansion of _X_.
  * Find _d_, the minimum _d_ such that _Ld_ is non-empty. If it doesn't exist 
then there is **no solution**.
  * _r = 2^d_.
  * if _r > minwidth_ then **no solution**.
  * else if _r = minwidth_.
    * Pop the minimum weight item from _Ld_ and insert it in _S_.
    * _X = X - minwidth_.
  * end if
  * _Pd+1 = PACKAGE(Ld)_.
  * Discard _Ld_.
  * _Ld+1 = MERGE(Pd+1, Ld+1)_.
* S is an optimal solution.

PACKAGE and MERGE refer to the following operations:
  * PACKAGE: Form the list _Pd+1_ by combining consecutive pairs of coins in the
set _Ld_, with coins ranked from smallest weight to greatest. If there is a
heaviest coin left alone at the end of the operation, it is simply discarded.
  * MERGE: Merge _Pd+1_ into the next set _Ld+1_, while maintaining the
non-decreasing weight order.

## References
1. Larmore, Lawrence L., and Daniel S. Hirschberg. "A fast algorithm for optimal length-limited Huffman codes." Journal of the ACM (JACM) 37.3 (1990): 464-473. https://doi.org/10.1145/79147.79150
1. Katajainen, Jyrki, Alistair Moffat, and Andrew Turpin. "A fast and space-economical algorithm for length-limited coding." International Symposium on Algorithms and Computation. Springer, Berlin, Heidelberg, 1995. https://doi.org/10.1007/BFb0015404
