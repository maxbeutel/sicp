#|

The tree for n=2 will look like this:

                 * {A, B, C, D, E, F} 63
              /    \
            F 32    * {A, B, C, D, E} 31
                  /   \
                E 16   * {A, B, C, D} 15
                     /   \
                   D 8    * {A, B, C} 7
                        /   \
                      C 4    * {A, B} 3
                           /   \
                         B 2    A 1

Bits for the most frequent: 1
Bits for the least frequent: n-1, we go done one level of the tree until we reach the last symbol.

|#
