# Package-merge algorithm Common Lisp implementation
We implemented the package-merge algorithm [1] for solving the problem of
length-limited Huffman coding.

⚠⚠⚠ This is an implementation for the sake of testing and research. Better
algorithms have been published for solving the exact same problem with much
better (space) performance [2]. If you use this in production, you are a 
**criminal**. ⚠⚠⚠

## Algorithm presentation
[1] introduces the (binary) "Coin Collector's problem". A numismat has run out
of money and must pay for his groceries with his coin collection. Each coin has
a face value and a numismatic value. The numismat wants to find the set of coins
that have a given total face value while minimizing the total numismatic value.

The coins can only have face values in (possibly negative) powers of two. The
problem is then suited to find sets of coins amounting to total sums _X_ that
are easily decomposed in a sum of powers of two.

Let us represent coins by the pair: (_i_, _w_).
* _i_: The coin has face value 2^i.
* _w_: Weight of the coin, its numismatic value.


## References
1. Larmore, Lawrence L., and Daniel S. Hirschberg. "A fast algorithm for optimal length-limited Huffman codes." Journal of the ACM (JACM) 37.3 (1990): 464-473. https://doi.org/10.1145/79147.79150
1. Katajainen, Jyrki, Alistair Moffat, and Andrew Turpin. "A fast and space-economical algorithm for length-limited coding." International Symposium on Algorithms and Computation. Springer, Berlin, Heidelberg, 1995. https://doi.org/10.1007/BFb0015404
