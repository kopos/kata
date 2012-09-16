CC=gcc
CFLAGS=-Wall -pedantic -std=iso9899:1999 -Wshadow -Wpointer-arith \
	-Wcast-qual -Wcast-align -Wstrict-prototypes -Wmissing-prototypes -Wconversion

edit_distance:	edit_distance.c
	$(CC) $(CFLAGS) edit_distance.c -o edit_distance

binom_coeff:	binom_coeff.c
	$(CC) $(CFLAGS) binom_coeff.c -o binom_coeff

fibonacci:	fibonacci.c
	$(CC) $(CFLAGS) fibonacci.c -o fibonacci

.PHONY: clean

clean:
	rm -f a.out fibonacci binom_coeff edit_distance
