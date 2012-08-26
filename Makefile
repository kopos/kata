CC=gcc
CFLAGS=-std=c99

fibonacci:	fibonacci.c
	$(CC) $(CFLAGS) fibonacci.c -o fibonacci

.PHONY: clean

clean:
	rm -f a.out fibonacci
