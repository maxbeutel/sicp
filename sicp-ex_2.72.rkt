#|

For each node in the tree, we need to check if the symbol to be encoded is in the list of symbols.
Assume this lookup takes O(n) time, where n=the number of symbols.

Assuming the frequency of symbols is as described in 2.71:

For the most frequent symbol, we only do this lookup one time, so it takes O(n) time to encode the most frequent symbol.

For the least frequent symbol, we need to do the lookup n-1 times, hence O(n * (n-1)) = O(n^2) time to encode the least frequent symbol.

|#
