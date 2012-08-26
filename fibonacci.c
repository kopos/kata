/**
 * Build: gcc fibonacci.c -std=c99
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

///
// Version 2: Recursive with caching
///
#define MAX 1000
#define UNKNOWN -1
#define DEBUG 0

int FIB[MAX];

void init() {
    for (int n = 0; n < MAX; ++n) {
        FIB[n] = UNKNOWN;
    }
}

int fib_dp(int n) {

    if (n == 0) { return 0; }
    if (n == 1) { return 1; }

    if (FIB[n] == UNKNOWN) {
#if DEBUG
        printf("Evaluating %d\n", n);
#endif

        int fib1 = fib_dp(n - 1);
        int fib2 = fib_dp(n - 2);
        FIB[n] = fib1 + fib2;
    }
    else {
#if DEBUG
        printf("Found %d\n", n);
#endif
    }

    return FIB[n];
}

///
// Version 3: Iterative with caching. Order of evaluation changed 
///
int fib_i(int n) {
    FIB[0] = 0;
    FIB[1] = 1;

    for(int i = 2; i < n; ++i) {
        int fib1 = FIB[n - 1];
        int fib2 = FIB[n - 2];
        FIB[i] = fib1 + fib2;
    }

    return FIB[n];
}

///
// Version 4: Iterative with 2 cache values - Depth of cache needed
///
int fib(int n) {
    int a = 0;
    int b = 1;

    for(int i = 2; i < n; ++i) {
        int c = a + b;
        a = b;
        b = c;
    }

    return b;
}

///
// Version 1: Pure recursive
///
int fib_r(int n) {
    if (n == 0) { return 0; }
    if (n == 1) { return 1; }

    return fib_r(n - 1) + fib_r(n - 2);
}

int main(int argc, char *argv[]) {
    int n = 0;
    if (argc > 1)
        n = atoi(argv[1]);
    init();

    printf("%d\n", fib_dp(n));

    return 0;
}
