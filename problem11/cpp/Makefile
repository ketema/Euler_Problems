CXX=g++
CXXFLAGS=-Wall -Wextra -O2

all: matrix_product

test: test_matrix_product
	./test_matrix_product

matrix_product: matrix_product.cpp
	$(CXX) $(CXXFLAGS) -o matrix_product matrix_product.cpp

test_matrix_product: test_matrix_product.cpp
	$(CXX) $(CXXFLAGS) -o test_matrix_product test_matrix_product.cpp

run: matrix_product
	./matrix_product matrix.txt

clean:
	rm -f matrix_product test_matrix_product
