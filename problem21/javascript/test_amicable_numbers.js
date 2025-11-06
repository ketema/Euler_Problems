#!/usr/bin/env node
/**
 * Tests for Project Euler Problem 21: Amicable Numbers
 */

const {
    sumOfProperDivisors,
    isAmicable,
    sumOfAmicableNumbers,
    findAmicableNumbers
} = require('./amicable_numbers.js');

// Simple test framework
class TestRunner {
    constructor() {
        this.passed = 0;
        this.failed = 0;
        this.tests = [];
    }

    assertEqual(actual, expected, message) {
        if (actual === expected) {
            this.passed++;
            console.log(`✓ ${message}`);
        } else {
            this.failed++;
            console.log(`✗ ${message}`);
            console.log(`  Expected: ${expected}`);
            console.log(`  Actual: ${actual}`);
        }
    }

    assertArrayEqual(actual, expected, message) {
        const match = actual.length === expected.length &&
                      actual.every((val, idx) => val === expected[idx]);
        if (match) {
            this.passed++;
            console.log(`✓ ${message}`);
        } else {
            this.failed++;
            console.log(`✗ ${message}`);
            console.log(`  Expected: [${expected.join(', ')}]`);
            console.log(`  Actual: [${actual.join(', ')}]`);
        }
    }

    assertTrue(condition, message) {
        if (condition) {
            this.passed++;
            console.log(`✓ ${message}`);
        } else {
            this.failed++;
            console.log(`✗ ${message}`);
        }
    }

    summary() {
        console.log(`\n${'='.repeat(50)}`);
        console.log(`Tests passed: ${this.passed}`);
        console.log(`Tests failed: ${this.failed}`);
        console.log(`Total tests: ${this.passed + this.failed}`);
        return this.failed === 0;
    }
}

// Run tests
const test = new TestRunner();

console.log('Testing sumOfProperDivisors...\n');

// Test small numbers
test.assertEqual(sumOfProperDivisors(1), 0, 'Divisors of 1');
test.assertEqual(sumOfProperDivisors(2), 1, 'Divisors of 2');
test.assertEqual(sumOfProperDivisors(3), 1, 'Divisors of 3');
test.assertEqual(sumOfProperDivisors(4), 1 + 2, 'Divisors of 4');
test.assertEqual(sumOfProperDivisors(6), 1 + 2 + 3, 'Divisors of 6');

// Test example from problem
test.assertEqual(sumOfProperDivisors(220), 284, 'Divisors of 220');
test.assertEqual(sumOfProperDivisors(284), 220, 'Divisors of 284');

// Test perfect numbers (d(n) = n)
test.assertEqual(sumOfProperDivisors(6), 6, 'Perfect number 6');
test.assertEqual(sumOfProperDivisors(28), 28, 'Perfect number 28');

console.log('\nTesting isAmicable...\n');

// Test known amicable pairs
test.assertTrue(isAmicable(220), '220 is amicable');
test.assertTrue(isAmicable(284), '284 is amicable');

// Perfect numbers are NOT amicable (a must not equal b)
test.assertTrue(!isAmicable(6), '6 is not amicable (perfect)');
test.assertTrue(!isAmicable(28), '28 is not amicable (perfect)');

// Test non-amicable numbers
test.assertTrue(!isAmicable(2), '2 is not amicable');
test.assertTrue(!isAmicable(100), '100 is not amicable');

console.log('\nTesting findAmicableNumbers...\n');

// Find amicable numbers under 300
const under300 = findAmicableNumbers(300);
test.assertArrayEqual(under300, [220, 284], 'Amicable numbers under 300');

// Find amicable numbers under 1000
const under1000 = findAmicableNumbers(1000);
test.assertTrue(under1000.includes(220), 'Under 1000 includes 220');
test.assertTrue(under1000.includes(284), 'Under 1000 includes 284');

console.log('\nTesting sumOfAmicableNumbers...\n');

// Test sum under 300
test.assertEqual(sumOfAmicableNumbers(300), 220 + 284, 'Sum under 300');

// Test sum under 1000
const sum1000 = sumOfAmicableNumbers(1000);
test.assertTrue(sum1000 >= 504, 'Sum under 1000 at least 504');

console.log('\nTesting solution for problem...\n');

// Test the actual problem
const result = sumOfAmicableNumbers(10000);
test.assertTrue(result > 0, 'Result is positive');
test.assertTrue(result > 1000, 'Result is reasonable for 10000 limit');

// Known answer
test.assertEqual(result, 31626, 'Correct answer for problem 21');

// Summary
const success = test.summary();
process.exit(success ? 0 : 1);
