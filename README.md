# rectpartitions
Algorithm that finds permutation of rows and colums that decomposises a matrix of 0 and 1 into minimal and exclusive set of covering rectangles

The task is to find a permutation of rows and columns of the matrix, such that a matrix built by shuffling the columns and rows according to the permutations can be partitioned into a minimal set of rectangles. Find all the rectangles as well.

For illustration, one can think about the problem this way:

>Suppose I have a set of objects and a set of properties. Each object can have any number of (distinct) properties. The task is to summarize (report) this mapping using the least amount of sentences. Each sentence has a form "`<list of objects> have properties <list of properties>`".


Brute-force the solution by applying the permutations and run the algorithm on each try have complexity that explodes exponentially making this approach non-practical for matrices bigger than 15×15. 

This problem feels like it is NP-hard, and there might be no fast (polynomial in time) solutions. What I present here is a heuristicall approximation, that is polinomial in time O(n^2) and good enough for my purposes.



It is worth noting that the problem is symmetric with respect to swapping rows with columns (features with objects). 

## The algorithm

Input: binary matrix, where rows are objects and columns are features and ones in the matrix represent matched pairs (object, feature). 

1. Find heuristically a good (or even a few good candidates) unshuffling permutation of rows and columns
2. Run 2D minimal rectangle decomposition to get the partitions. 

### Minimal rectangle decomposition

The algorithm will take several steps. 

1. Decompose the matrix into sets of contiguous islands of ones. Form a list of the islands and then work with each island separately. (It is at most O(n^2) problem, n = total size of the matrix, but I believe it can be optimized further). 
2. Compress each island by eliminating duplicated rows and columns, and store this information with the matrix, so I will be able to revert this process. This step is optional, but it should speed up the rest significantly, as it will also take care of rows and columns filled with zeroes, reducing sizes of the matrices considerably, especially if there are more zeroes then ones in the matrix.
3. Find the biggest rectangle in the island using a modified version of any of the solutions to the *maximal rectangle problem*, which are also O(n). The modification will account for the weights of the rows and columns introduced by compression step. Remember the solution, and cover it with zeroes in the matrix. (O(n)). 
4. Decompose the matrix into the contiguous islands of 1 (like in step 1) and add them to a list of islands and jump to the step 2.
5. When there are no more islands to work with, return the solution as a list of rectangles found in step 3.

### Unshuffling the rows (and columns)

Unshuffling rows are independent of unshuffling columns and both tasks can be run separately (concurrently). Let us assume I am looking for the unshuffling permutation of columns.

Also, it is worth noting, that unshuffling a matrix should yield the same results if we swap ones with zeroes. 

1. Build a distance matrix of columns. A distance between two columns is defined as Manhattan distance between the two columns represented numerically (i.e. 0 - the absence of a relationship between object and feature, 1 - presence)
2. Run hierarchical clustering using the distance matrix. The complexity is O(n^2), as I believe single linkage should be good enough. 
3. The order of objects returned from the hierarchical clustering is the unshuffling permutation.
