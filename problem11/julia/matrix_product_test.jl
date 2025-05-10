using Test
include("matrix_product.jl")
using .MatrixProduct

@testset "greatest_product 4x4" begin
    matrix = [1 2 3 4;
              5 6 7 8;
              9 10 11 12;
              13 14 15 16]
    result, coords = greatest_product(matrix, 4)
    expected = 13 * 14 * 15 * 16
    expected_coords = [(4,1), (4,2), (4,3), (4,4)]
    @test result == expected
    @test coords == expected_coords
end
