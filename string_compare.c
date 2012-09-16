#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef uint
#define uint unsigned int
#endif

#define INSERT  1
#define DELETE  1
#define REPLACE 1

///
// Function prototypes
///
int min(int, int);
int min3(int, int, int);
int string_compare(char *, char *, uint, uint);
int ins(char);
int del(char);
int match(char, char);

int ins(char c) { return 1; }
int del(char c) { return 1; }
int match(char c1, char c2) { return 2; }

int min(int a, int b) {
    return (a < b)? a: b;
}

int min3(int a, int b, int c) {
    return min(min(a, b), c);
}

int string_compare(char *s1, char *s2, uint l1, uint l2) {
    if (l1 == 0) { return (int) l2 * del(' '); }
    if (l2 == 0) { return (int) l1 * ins(' '); }

    printf("%.*s %.*s\n", l1, s1, l2, s2);
    int c1 = string_compare(s1, s2, l1 - 1, l2) + ins(s1[l1]);
    int c2 = string_compare(s1, s2, l1, l2 - 1) + del(s2[l2]);
    int c3 = string_compare(s1, s2, l1 - 1, l2 - 1) + match(s1[l1], s2[l2]);

    printf("%.*s %.*s %d %d %d %d %d\n", l1, s1, l2, s2, l1, l2, c1, c2, c3);
    return min3(c1, c2, c3);
}

int main(int argc, char **argv) {
    char s1[] = "ab"; //"will";
    char s2[] = "b"; //"willis";
    printf("edit distance: %d\n", string_compare(s1, s2, strlen(s1), strlen(s2)));

    return 0;
}
