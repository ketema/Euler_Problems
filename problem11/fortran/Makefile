# Makefile for Fortran Matrix Product Solution

FC=gfortran
FFLAGS=-O2
TARGET=matrix_product
SRC=matrix_product.f90

.PHONY: all clean test

all: $(TARGET)

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) -o $(TARGET) $(SRC)

test: $(TARGET)
	./$(TARGET) matrix.txt

clean:
	rm -f $(TARGET) *.o *.mod

