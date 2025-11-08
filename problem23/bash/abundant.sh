#!/bin/bash
# Project Euler Problem 23: Non-abundant Sums
# Bash implementation following TDD methodology

# Calculate sum of proper divisors of n
# Proper divisors are all divisors less than n
sum_of_divisors() {
    local n=$1

    if [ "$n" -le 1 ]; then
        echo 0
        return
    fi

    local sum=1  # 1 is always a proper divisor for n > 1
    local sqrt_n=$(echo "sqrt($n)" | bc)

    # Check divisors up to sqrt(n)
    for ((i=2; i<=sqrt_n; i++)); do
        if [ $((n % i)) -eq 0 ]; then
            sum=$((sum + i))

            # Add the corresponding divisor (n/i) if it's different from i
            local other=$((n / i))
            if [ "$other" -ne "$i" ] && [ "$other" -ne "$n" ]; then
                sum=$((sum + other))
            fi
        fi
    done

    echo "$sum"
}

# Check if a number is abundant
# Returns 0 (true) if abundant, 1 (false) otherwise
is_abundant() {
    local n=$1
    local divisor_sum=$(sum_of_divisors "$n")

    if [ "$divisor_sum" -gt "$n" ]; then
        return 0  # true - is abundant
    else
        return 1  # false - not abundant
    fi
}

# Find all abundant numbers up to limit
find_abundant_numbers() {
    local limit=$1
    local abundant_nums=""

    for ((n=1; n<=limit; n++)); do
        if is_abundant "$n"; then
            abundant_nums="$abundant_nums$n "
        fi
    done

    echo "$abundant_nums"
}

# Check if a number can be written as sum of two abundant numbers
can_be_sum_of_two_abundant() {
    local n=$1
    local abundant_list=$2

    # Convert abundant_list to array
    local abundant_array=($abundant_list)

    # Check all pairs
    for abundant1 in "${abundant_array[@]}"; do
        # If abundant1 > n, we can stop
        if [ "$abundant1" -ge "$n" ]; then
            break
        fi

        local needed=$((n - abundant1))

        # Check if needed is in the abundant list
        for abundant2 in "${abundant_array[@]}"; do
            if [ "$abundant2" -eq "$needed" ]; then
                return 0  # true - can be written as sum
            fi
            if [ "$abundant2" -gt "$needed" ]; then
                break
            fi
        done
    done

    return 1  # false - cannot be written as sum
}

# Main solution function
solve_problem() {
    local limit=${1:-28123}

    echo "Finding abundant numbers up to $limit..." >&2
    local abundant_nums=$(find_abundant_numbers "$limit")

    echo "Calculating sum of non-abundant sums..." >&2
    local total=0

    for ((n=1; n<=limit; n++)); do
        if ! can_be_sum_of_two_abundant "$n" "$abundant_nums"; then
            total=$((total + n))
        fi

        # Progress indicator
        if [ $((n % 1000)) -eq 0 ]; then
            echo "  Progress: $n/$limit" >&2
        fi
    done

    echo "$total"
}

# If script is run directly (not sourced), execute main function
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    result=$(solve_problem "$@")
    echo ""
    echo "Problem 23 Answer: $result"
fi
