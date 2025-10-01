#!/bin/bash

echo "=== Solution File Statistics ==="
echo ""

# C
echo "C:"
echo "  Solution: $(wc -l < c/matrix_product.c) lines"
echo "  Test: $(wc -l < c/matrix_product_test.c) lines"

# C++
echo "C++:"
echo "  Solution: $(wc -l < cpp/matrix_product.cpp) lines"
echo "  Test: $(wc -l < cpp/matrix_product_test.cpp) lines"

# C#
echo "C#:"
echo "  Solution: $(wc -l < csharp/MatrixProduct.cs) lines"
echo "  Test: $(wc -l < csharp/MatrixProductTests.cs) lines"

# Fortran
echo "Fortran:"
echo "  Solution: $(wc -l < fortran/matrix_product.f90) lines"
echo "  Test: N/A"

# Go
echo "Go:"
echo "  Solution: $(wc -l < go/matrix_product.go) lines"
echo "  Test: $(wc -l < go/matrix_product_test.go) lines"

# Haskell
echo "Haskell:"
echo "  Solution: $(wc -l < haskell/app/Main.hs) lines"
echo "  Test: $(wc -l < haskell/test/Spec.hs) lines"

# Java
echo "Java:"
echo "  Solution: $(wc -l < java/src/main/java/MatrixProduct.java) lines"
echo "  Test: $(wc -l < java/src/test/java/MatrixProductTest.java) lines"

# Julia
echo "Julia:"
echo "  Solution: $(wc -l < julia/matrix_product.jl) lines"
echo "  Test: $(wc -l < julia/matrix_product_test.jl) lines"

# Kotlin
echo "Kotlin:"
echo "  Solution: $(wc -l < kotlin/src/main/kotlin/MatrixProduct.kt) lines"
echo "  Test: $(wc -l < kotlin/src/test/kotlin/MatrixProductTest.kt) lines"

# Perl
echo "Perl:"
echo "  Solution: $(wc -l < perl/matrix_product.pl) lines"
echo "  Test: $(wc -l < perl/matrix_product_test.pl) lines"

# PHP
echo "PHP:"
echo "  Solution: $(wc -l < php/solution.php) lines"
echo "  Test: $(wc -l < php/tests/MatrixProductTest.php) lines"

# Python
echo "Python:"
echo "  Solution: $(wc -l < python/matrix_product.py) lines"
echo "  Test: $(wc -l < python/test_matrix_product.py) lines"

# Ruby
echo "Ruby:"
echo "  Solution: $(wc -l < ruby/matrix_product.rb) lines"
echo "  Test: $(wc -l < ruby/matrix_product_test.rb) lines"

# Rust
echo "Rust:"
echo "  Solution: $(wc -l < rust/src/main.rs) lines"
echo "  Test: $(wc -l < rust/tests/integration_test.rs) lines"

# Scala
echo "Scala:"
echo "  Solution: $(wc -l < scala/src/main/scala/MatrixProduct.scala) lines"
echo "  Test: $(wc -l < scala/src/test/scala/MatrixProductTest.scala) lines"

# Swift
echo "Swift:"
echo "  Solution: $(wc -l < swift/matrix_product.swift) lines"
echo "  Test: $(wc -l < swift/matrix_product_tests.swift) lines"

# TypeScript
echo "TypeScript:"
echo "  Solution: $(wc -l < typescript/matrixProduct.ts) lines"
echo "  Test: $(wc -l < typescript/matrixProduct.test.ts) lines"
