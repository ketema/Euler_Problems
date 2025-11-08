-- Project Euler Problem 24: Lexicographic Permutations
-- Lua implementation following TDD methodology

-- Calculate factorial of n
function factorial(n)
    if n <= 1 then
        return 1
    end

    local result = 1
    for i = 2, n do
        result = result * i
    end
    return result
end

-- Find the nth lexicographic permutation of the given elements
-- Uses factorial number system for efficiency: O(n²) instead of O(n!)
-- n is 1-indexed (1st permutation, 2nd permutation, etc.)
function nth_permutation(elements, n)
    -- Make a copy of elements so we don't modify the original
    local available = {}
    for i = 1, #elements do
        available[i] = elements[i]
    end

    local result = {}
    local k = n - 1  -- Convert to 0-indexed for math

    -- For each position in the result
    for i = 1, #elements do
        local remaining = #elements - i
        local fact = factorial(remaining)

        -- Determine which element to pick
        local index = math.floor(k / fact) + 1
        table.insert(result, available[index])

        -- Remove the picked element from available
        table.remove(available, index)

        -- Update k for the remaining elements
        k = k % fact
    end

    return result
end

-- Convert permutation array to string
function perm_to_string(perm)
    local result = ""
    for i = 1, #perm do
        result = result .. tostring(perm[i])
    end
    return result
end

-- Main solution function
function solve(n)
    n = n or 1000000
    local digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
    local perm = nth_permutation(digits, n)
    return perm_to_string(perm)
end

-- If run as a script (not required), execute solve
if arg and arg[0]:match("lexicographic_perm%.lua$") then
    local n = tonumber(arg[1]) or 1000000
    local result = solve(n)
    print(string.format("\nProblem 24: Lexicographic Permutations"))
    print(string.format("Finding the %dth permutation of 0-9", n))
    print(string.format("\nAnswer: %s", result))
end
