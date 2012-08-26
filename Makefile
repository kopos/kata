CC=gcc
CFLAGS=-std=c99

binom_coeff:	binom_coeff.c
	$(CC) $(CFLAGS) binom_coeff.c -o binom_coeff

fibonacci:	fibonacci.c
	$(CC) $(CFLAGS) fibonacci.c -o fibonacci

.PHONY: clean

clean:
	rm -f a.out fibonacci binom_coeff
