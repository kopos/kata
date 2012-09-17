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

#define MATCH  0
#define INSERT 1
#define DELETE 2

#define MAX 32

int ed[MAX][MAX];

typedef struct {
    int cost;
    int parent;
} cell;
cell m[MAX + 1][MAX + 1];

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
int edit_distance3(char *, char *);
void row_init(int);
void column_init(int);
void goal_cell(char *, char *, int *, int *);

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
    return 1;
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

///
// Version 3: Dynamic programming version
///
int edit_distance3(char *s1, char *s2) {
    int i, j;
    int opt[3];

    for (i = 0; i < MAX; ++i) {
        row_init(i);
        column_init(i);
    }

    for (i = 1; i < strlen(s1); ++i) {
        for (j = 1; j < strlen(s2); ++j) {
            opt[MATCH] = m[i - 1][j - 1].cost + match(s1[i], s2[j]);
            opt[INSERT] = m[i][j - 1].cost + del(s2[j]);
            opt[DELETE] = m[i - 1][j].cost + ins(s1[i]);

            m[i][j].cost = opt[MATCH];
            m[i][j].parent = MATCH;
            for(int k = INSERT; k <= DELETE; ++k) {
                if (opt[k] < m[i][j].cost) {
                    m[i][j].cost = opt[k];
                    m[i][j].parent = k;
                }
            }
        }
    }

    goal_cell(s1, s2, &i, &j);
    return m[i][j].cost;
}

void row_init(int i) {
    m[0][i].cost = i;

    if (i > 0)
        m[0][i].parent = INSERT;
    else
        m[0][i].parent = -1;
}

void column_init(int i) {
    m[i][0].cost = i;

    if (i > 0)
        m[i][0].parent = DELETE;
    else
        m[i][0].parent = -1;
}

void goal_cell(char *s1, char *s2, int *i, int *j) {
    *i = (int) strlen(s1) - 1;
    *j = (int) strlen(s2) - 1;
}

int main(int argc, char **argv) {
    char s1[] = "adam smith";
    char s2[] = "will smith";

    init(s1, s2);

    //printf("edit distance: %d\n", edit_distance2(s1, s2, strlen(s1), strlen(s2)));
    printf("edit distance: %d\n", edit_distance3(s1, s2));

    return 0;
}
