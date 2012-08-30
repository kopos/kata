/**
 * To build a program to calculate the Binomial coefficient (n, k) such that it
 * counts the number of ways to choose k things out of n possibilities.
 * (n, k) = n! / ((n-k)! * k!)
 *
 * http://math.berkeley.edu/~rbayer/09su-55/handouts/permcombbin.pdf
 * 
 * Permutation
 * -----------
 * An ordering of a set is a pemutation of that set. An ordering of
 * k-elements of a set is called a k-permutation.
 *
 * n * (n - 1) * (n - 2) * ... * (n - (k - 1)) = n! / (n - k)!
 *
 * Combination
 * -----------
 * Picking k elements from a set in which order doesn't matter is called a 
 * k-combination. Ordering k elements and then removing the k! similar orderings.
 *
 * n * (n - 1) * (n - 2) * ... * (n - (k - 1)) / k * (k - 1) * (k - 2) * ...  1
 * = n! / ((n - k)! * k!)
 *
 * (n, k) aka "n choose k" stands for n! / ((n - k)! * k!) and is the number of
 * ways to pick k elements from an n element set if order doesn't matter.
 *
 * Binomial Coefficient
 * --------------------
 * Coefficient of x^3 y^8 in (x + y)^11 is different ways to pick 8 positions from
 * 11 available positions = 11 C 8.
 *
 * Combinatorial proof:
 * n C k = (n - 1) C (k - 1) + (n - 1) C k
 * RHS = Number of subsets containing an element 'a' + Number of subsets not
 *       containing element 'a'
 */
#include <stdio.h>
#include <stdlib.h>
#define MAX 100

long bc[MAX][MAX];

///
// Function Prototypes
///
long factorial(int);
long binom_coeff(int, int);
long binom_coeff_r(int, int);
void init(int, int);
long binom_coeff_cache(int, int);
long bingo_coeff_dp(int, int);

long factorial(int n) {
    long f = 1;

    while (n > 1) {
        f *= n;
        --n;
    }

    return f;
}

///
// Version 1
///
long binom_coeff(int n, int k) {
    return factorial(n) / (factorial(n - k) * factorial(k));
}

///
// Version 2: Based on proof from pdf above
// (n + 1, k) = (n, k - 1) + (n, k)
///
long binom_coeff_r(int n, int k) {
    if (k == 0) { return 1; }
    if (n == k) { return 1; }

    return binom_coeff_r(n - 1, k - 1) + binom_coeff_r(n - 1, k);
}

///
// Version 3: Using a table to cache results. Still recursive.
///
void init(int n, int k) {
    for (int i = 0; i <= n; ++i) {
        for (int j = 0; j <= k; ++j) {
            bc[i][j] = -1;
        }
    }

    for (int i = 0; i <= n; ++i) {
        bc[i][0] = 1;
        bc[i][i] = 1;

        bc[i][1] = i;
    }
}

long binom_coeff_cache(int n, int k) {
    if (bc[n][k] == -1) {
        bc[n][k] = binom_coeff_cache(n - 1, k - 1) +
            binom_coeff_cache(n - 1, k);
    }

    return bc[n][k];
}

/**
 * Version 4: Using the look up table as well as iterative
 * A B
 *  \|
 *   X
 */
long bingo_coeff_dp(int n, int k) {
    for (int i = 0; i <=n; ++i) {
        for (int j = k; j > 0; --j) {
            bc[i][j] = bc[i - 1][j - 1] + bc[i - 1][j];
        }
    }

    return bc[n][k];
}

int main(int argc, char **argv) {
    int n = 0, k = 0;
    if (argc > 2) {
        n = atoi(argv[1]);
        k = atoi(argv[2]);
    }

    init(n, k);

    printf("%ld\n", binom_coeff_cache(n, k));

    return 0;
}
