module MatrixProduct

export greatest_product

function greatest_product(matrix::Array{Int,2}, adj::Int)
    rows, cols = size(matrix)
    max_prod = 0
    best_coords = Vector{Tuple{Int,Int}}()
    for i in 1:rows
        for j in 1:cols
            # right
            if j + adj - 1 <= cols
                prod = 1
                coords = Vector{Tuple{Int,Int}}()
                for k in 0:adj-1
                    prod *= matrix[i, j+k]
                    push!(coords, (i, j+k))
                end
                if prod > max_prod
                    max_prod = prod
                    best_coords = coords
                end
            end
            # down
            if i + adj - 1 <= rows
                prod = 1
                coords = Vector{Tuple{Int,Int}}()
                for k in 0:adj-1
                    prod *= matrix[i+k, j]
                    push!(coords, (i+k, j))
                end
                if prod > max_prod
                    max_prod = prod
                    best_coords = coords
                end
            end
            # diag down-right
            if i + adj - 1 <= rows && j + adj - 1 <= cols
                prod = 1
                coords = Vector{Tuple{Int,Int}}()
                for k in 0:adj-1
                    prod *= matrix[i+k, j+k]
                    push!(coords, (i+k, j+k))
                end
                if prod > max_prod
                    max_prod = prod
                    best_coords = coords
                end
            end
            # diag down-left
            if i + adj - 1 <= rows && j - adj + 1 >= 1
                prod = 1
                coords = Vector{Tuple{Int,Int}}()
                for k in 0:adj-1
                    prod *= matrix[i+k, j-k]
                    push!(coords, (i+k, j-k))
                end
                if prod > max_prod
                    max_prod = prod
                    best_coords = coords
                end
            end
        end
    end
    return max_prod, best_coords
end

end # module
