#!/usr/bin/env node
/**
 * Project Euler Problem 21: Amicable Numbers
 *
 * Let d(n) be defined as the sum of proper divisors of n
 * (numbers less than n which divide evenly into n).
 *
 * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair
 * and each of a and b are called amicable numbers.
 *
 * Example: d(220) = 284 and d(284) = 220
 *
 * Find the sum of all the amicable numbers under 10000.
 */

/**
 * Calculate sum of proper divisors of n
 * (all divisors less than n)
 *
 * @param {number} n - The number to find divisors for
 * @returns {number} Sum of proper divisors
 */
function sumOfProperDivisors(n) {
    if (n <= 1) return 0;

    let sum = 1; // 1 is always a proper divisor for n > 1

    // Check divisors up to sqrt(n)
    const sqrt = Math.sqrt(n);
    for (let i = 2; i <= sqrt; i++) {
        if (n % i === 0) {
            sum += i;
            // Add the corresponding divisor (n/i) if it's different
            const otherDivisor = n / i;
            if (otherDivisor !== i) {
                sum += otherDivisor;
            }
        }
    }

    return sum;
}

/**
 * Check if a number is amicable
 *
 * @param {number} a - Number to check
 * @returns {boolean} True if a is amicable
 */
function isAmicable(a) {
    const b = sumOfProperDivisors(a);

    // a and b must be different
    if (a === b) return false;

    // Check if d(b) = a
    const c = sumOfProperDivisors(b);
    return c === a;
}

/**
 * Find sum of all amicable numbers under limit
 *
 * @param {number} limit - Upper bound (exclusive)
 * @returns {number} Sum of all amicable numbers under limit
 */
function sumOfAmicableNumbers(limit) {
    let sum = 0;

    for (let n = 2; n < limit; n++) {
        if (isAmicable(n)) {
            sum += n;
        }
    }

    return sum;
}

/**
 * Find all amicable numbers under limit
 *
 * @param {number} limit - Upper bound (exclusive)
 * @returns {number[]} Array of amicable numbers
 */
function findAmicableNumbers(limit) {
    const amicable = [];

    for (let n = 2; n < limit; n++) {
        if (isAmicable(n)) {
            amicable.push(n);
        }
    }

    return amicable;
}

// Export for testing
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        sumOfProperDivisors,
        isAmicable,
        sumOfAmicableNumbers,
        findAmicableNumbers
    };
}

// Run if executed directly
if (require.main === module) {
    console.log('Solving Project Euler Problem 21...');
    console.log('Finding amicable numbers under 10000\n');

    // Verify example from problem
    const d220 = sumOfProperDivisors(220);
    const d284 = sumOfProperDivisors(284);

    console.log(`d(220) = ${d220} (expected 284)`);
    console.log(`d(284) = ${d284} (expected 220)`);
    console.log(`220 and 284 are amicable: ${isAmicable(220) && isAmicable(284)}\n`);

    // Find all amicable numbers under 10000
    const amicableNumbers = findAmicableNumbers(10000);
    console.log(`Amicable numbers under 10000:`);
    console.log(amicableNumbers.join(', '));
    console.log();

    // Calculate sum
    const result = sumOfAmicableNumbers(10000);
    console.log(`Problem 21 Answer: ${result}`);
}
