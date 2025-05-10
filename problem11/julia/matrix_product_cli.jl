include("matrix_product.jl")
using .MatrixProduct

function read_matrix(filename::String)
    open(filename, "r") do io
        return [parse.(Int, split(strip(line))) for line in eachline(io) if !isempty(strip(line))]
    end
end

function print_matrix_with_highlight(matrix::Array{Int,2}, coords::Vector{Tuple{Int,Int}})
    coordset = Set(coords)
    rows, cols = size(matrix)
    for i in 1:rows
        for j in 1:cols
            val = matrix[i, j]
            if (i, j) in coordset
                print("\e[31m", lpad(val, 2, '0'), "\e[0m ")
            else
                print(lpad(val, 2, '0'), " ")
            end
        end
        println()
    end
end

function main()
    filename = length(ARGS) > 0 ? ARGS[1] : "matrix.txt"
    mat = read_matrix(filename)
    matrix = reduce(vcat, [row' for row in mat])
    result, coords = greatest_product(matrix, 4)
    print_matrix_with_highlight(matrix, coords)
    println("Greatest product of four adjacent numbers: $result")
end

main()
