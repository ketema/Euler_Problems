def is_sum_of_digit_powers(n, power):
    return n == sum(int(digit)**power for digit in str(n))

def solve(power):
    # Determine a safe upper bound.
    # Find the largest d for which d * 9^power has d digits.
    # A simpler way is to find d such that 10^(d-1) > d * 9^power.
    # For power 4, d=6 -> 10^5 > 6*9^4 = 39366. So check up to 5 digits.
    # max sum is 5 * 9^4 = 32805. Let's use 100000 as a safe upper bound.
    # For power 5, d=7 -> 10^6 > 7*9^5 = 413343. So check up to 6 digits.
    # max sum is 6 * 9^5 = 354294. Let's use 400000 as a safe upper bound.
    upper_bound = 0
    if power == 4:
        upper_bound = 100000
    elif power == 5:
        upper_bound = 400000

    total_sum = 0
    for i in range(2, upper_bound):
        if is_sum_of_digit_powers(i, power):
            total_sum += i
    return total_sum

if __name__ == '__main__':
    print(solve(5))
