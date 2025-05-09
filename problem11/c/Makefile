CC=gcc
CFLAGS=-Wall -Wextra -O2

all: matrix_product

test: test_matrix_product
	./test_matrix_product

matrix_product: matrix_product.c
	$(CC) $(CFLAGS) -o matrix_product matrix_product.c

test_matrix_product: test_matrix_product.c
	$(CC) $(CFLAGS) -o test_matrix_product test_matrix_product.c

run: matrix_product
	./matrix_product matrix.txt

clean:
	rm -f matrix_product test_matrix_product
