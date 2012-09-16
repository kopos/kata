/**
 * Write a program to find the edit-distance of 2 strings.
 *
 * Basic steps of transformation
 * - INSERT
 * - DELETE
 * - MATCH / REPLACE
 *
 * The basic recursive algo simply tries to built and find the best of the 3 
 * possible execution paths (simple insert, simple delete, simple match) for
 * the 2 strings. And recursively building this process.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef uint
#define uint unsigned int
#endif

#define MAX 32

int ed[MAX][MAX];

///
// Function prototypes
///
int min(int, int);
int min3(int, int, int);
int ins(char);
int del(char);
int match(char, char);
int edit_distance(char *, char *, uint, uint);
int edit_distance2(char *, char *, uint, uint);
void init(char *, char *);

///
// insert cost
///
int ins(char c) { return 1; }

///
// delete cost
///
int del(char c) { return 1; }

///
// replace / match cost
///
int match(char c1, char c2) {
    if (c1 == c2) return 0;
    return 2;
}

int min(int a, int b) {
    return (a < b)? a: b;
}

int min3(int a, int b, int c) {
    return min(min(a, b), c);
}

///
// Version 1
///
int edit_distance(char *s1, char *s2, uint l1, uint l2) {
    if (l1 == 0) { return (int) l2 * del(' '); }
    if (l2 == 0) { return (int) l1 * ins(' '); }

#ifdef DEBUG
    printf("%.*s, %.*s\n", l1, s1, l2, s2);
#endif
    int c1 = edit_distance(s1, s2, l1 - 1, l2) + ins(s1[l1]);
    int c2 = edit_distance(s1, s2, l1, l2 - 1) + del(s2[l2]);
    int c3 = edit_distance(s1, s2, l1 - 1, l2 - 1) + match(s1[l1], s2[l2]);
    int c = min3(c1, c2, c3);
#ifdef DEBUG
    printf("edit_distance(%.*s, %.*s) = %d\n", l1, s1, l2, s2, c);
#endif

    return c;
}

///
// Version 2: Memoized version
///
int edit_distance2(char *s1, char *s2, uint l1, uint l2) {
    if (ed[l1][l2] != -1) {
        return ed[l1][l2];
    }

    if (l1 == 0) { return (int) l2 * del(' '); }
    if (l2 == 0) { return (int) l1 * ins(' '); }

    int c1 = edit_distance2(s1, s2, l1 - 1, l2) + ins(s1[l1]);
    int c2 = edit_distance2(s1, s2, l1, l2 - 1) + del(s2[l2]);
    int c3 = edit_distance2(s1, s2, l1 - 1, l2 - 1) + match(s1[l1], s2[l2]);

    int c = min3(c1, c2, c3);
    ed[l1][l2] = c;

    return c;

}

void init(char *s1, char *s2) {
    for (int i = 0; i < strlen(s1) + 1; ++i)
        for (int j = 0; j < strlen(s2) + 1; ++j)
            ed[i][j] = -1;
}

int main(int argc, char **argv) {
    char s1[] = "smith will";
    char s2[] = "bruce willis";

    init(s1, s2);

    printf("edit distance: %d\n", edit_distance2(s1, s2, strlen(s1), strlen(s2)));

    return 0;
}
