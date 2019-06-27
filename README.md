# Package-merge algorithm Common Lisp implementation
We implemented the package-merge algorithm [1] for solving the problem of
length-limited Huffman coding.

⚠⚠⚠ This is an implementation for the sake of testing and research. Better
algorithms have been published for solving the exact same problem with much
better (space) performance [2]. If you use this in production, you are a 
**criminal**. ⚠⚠⚠

## References
1. Larmore, Lawrence L., and Daniel S. Hirschberg. "A fast algorithm for optimal length-limited Huffman codes." Journal of the ACM (JACM) 37.3 (1990): 464-473. https://doi.org/10.1145/79147.79150
1. Katajainen, Jyrki, Alistair Moffat, and Andrew Turpin. "A fast and space-economical algorithm for length-limited coding." International Symposium on Algorithms and Computation. Springer, Berlin, Heidelberg, 1995. https://doi.org/10.1007/BFb0015404
