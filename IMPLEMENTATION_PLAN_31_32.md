# Implementation Plan: Project Euler #31 and #32

## Overview
Solve Project Euler problems #31 (Coin Sums) and #32 (Pandigital Products) using Python with full constitutional TDD adherence.

## Problem Definitions

### Problem #31: Coin Sums
**Question**: How many different ways can £2 (200 pence) be made using any number of UK coins?

**Coins Available**: 1p, 2p, 5p, 10p, 20p, 50p, 100p (£1), 200p (£2)

**Approach**: Dynamic programming (coin change problem)
- Classic DP problem: count ways to make target sum with given denominations
- Use bottom-up DP table where dp[i] = number of ways to make amount i
- For each coin denomination, update all amounts that can be formed

**Expected Answer**: Need to calculate (deterministic result)

### Problem #32: Pandigital Products
**Question**: Find the sum of all products whose multiplicand/multiplier/product identity uses digits 1-9 exactly once.

**Example**: 39 × 186 = 7254 is pandigital (391867254 contains 1-9 exactly once)

**Approach**: Brute force with validation
- Constraints: multiplicand × multiplier = product must use digits 1-9 exactly once
- Patterns: Can be 1-digit × 4-digit = 4-digit OR 2-digit × 3-digit = 4-digit
- For each candidate product, check if concatenated string is pandigital
- Use set to avoid duplicate products (hint: some products obtainable multiple ways)

**Expected Answer**: Need to calculate (deterministic result)

## Technical Design

### Directory Structure
```
problem31/
  python/
    src/
      coin_sums.py          # Implementation
    tests/
      test_coin_sums.py     # Tests
    requirements.txt        # pytest, pytest-cov
    pytest.ini              # Test configuration

problem32/
  python/
    src/
      pandigital_products.py  # Implementation
    tests/
      test_pandigital_products.py  # Tests
    requirements.txt        # pytest, pytest-cov
    pytest.ini              # Test configuration
```

### Design Patterns

**Problem #31**: Dynamic Programming (PoEAA: Transaction Script)
- Pure function: `count_coin_combinations(target: int, coins: List[int]) -> int`
- No side effects, deterministic
- O(n*m) time, O(n) space where n=target, m=num_coins

**Problem #32**: Filter-Map-Reduce (Functional Pattern)
- Pure function: `sum_pandigital_products() -> int`
- Helper: `is_pandigital(multiplicand: int, multiplier: int, product: int) -> bool`
- No side effects, deterministic

**Shared Utilities** (if needed):
- If digit manipulation functions are common between problems, create shared utils.py
- DRY principle: Avoid duplicating validation or helper logic
- Current assessment: Problems are sufficiently different (no immediate sharing needed)

### Constitutional Adherence

**CL6 TDD Enforcement**:
- RED phase: Write tests FIRST with self-documenting error messages (5-point standard)
- GREEN phase: Implement to pass tests
- COMMIT: WHY/EXPECTED format
- REFACTOR: If needed

**QS1 Test Requirements**:
- >85% coverage
- Edge cases: 0, negative inputs, boundary conditions
- Exact value validation (no ranges for deterministic problems)
- Self-documenting error messages per euler-28 lessons

**QS2 Design**:
- DRY: Reusable helper functions
- Functional: Pure functions, immutable data, explicit errors
- No hardcoded values (except problem constraints like coin denominations)

## Test Strategy

### Problem #31 Tests
1. **Small cases** (exact validation):
   - 1 pence → 1 way
   - 2 pence → 2 ways (1+1, 2)
   - 5 pence → 4 ways (verify manually)
2. **Edge cases**:
   - 0 pence → 1 way (empty set)
   - Negative amount → raise ValueError
3. **Production case**:
   - 200 pence → EXACT expected value (calculate independently)

### Problem #32 Tests
1. **Known pandigital**:
   - 39 × 186 = 7254 → is_pandigital returns True
2. **Non-pandigital**:
   - 12 × 34 = 408 → is_pandigital returns False (missing digits)
   - Repeated digits: 11 × 11 = 121 → False
   - Contains zero: 10 × 10 = 100 → False
3. **Duplicate handling**:
   - Test that products obtainable multiple ways are counted once (use set)
4. **Production case**:
   - sum_pandigital_products() → EXACT expected value (calculate independently)

## Independent Verification Process

**Problem #31: Expected Answer Verification**:
1. Manually calculate small cases (2p, 5p) to verify DP logic
2. Cross-reference with known Project Euler solutions (if available)
3. Verify algorithm against standard coin change DP implementations

**Problem #32: Expected Answer Verification**:
1. Manually enumerate small pandigital products (1×4, 2×3 patterns)
2. Verify example case (39 × 186 = 7254)
3. Cross-reference with Project Euler forums (after solving)
4. Double-check set usage for duplicate elimination

## Coverage Enforcement

**Tool**: pytest-cov

**Configuration** (pytest.ini):
```ini
[pytest]
testpaths = tests
python_files = test_*.py
python_classes = Test*
python_functions = test_*
addopts = --cov=src --cov-report=term-missing --cov-fail-under=85
```

**Usage**:
```bash
pytest --cov=src --cov-report=term-missing --cov-fail-under=85
```

**Requirements**:
- Minimum 85% coverage
- All functions tested (including edge cases)
- Coverage report shows missing lines

## Risk Mitigation

**From euler-28-adversarial-tdd-lessons**:
1. ✓ Verify expected values independently before writing tests
2. ✓ Error messages describe BEHAVIOR (WHAT) not implementation (HOW)
3. ✓ Use exact validation for deterministic problems
4. ✓ No hardcoded workarounds in implementation

**Problem #31 Risks**:
- Risk: Incorrect DP recurrence relation
- Mitigation: Verify with manual calculation for small cases (2p, 5p)

**Problem #32 Risks**:
- Risk: Missing product patterns (1×4, 2×3 digit combinations)
- Mitigation: Exhaustive search within bounds, verify no valid products missed

## Success Criteria

**Correctness**:
- ✓ Problem #31: Correct count of ways to make £2
- ✓ Problem #32: Correct sum of pandigital products

**Constitutional Compliance**:
- ✓ All tests pass (100%)
- ✓ Coverage >85%
- ✓ TDD cycle followed (RED→GREEN→COMMIT for each problem)
- ✓ Evidence format: F:path T:test C:hash COV:% O:output
- ✓ Response template used
- ✓ Git commits with WHY/EXPECTED format

## Timeline

1. ✓ M3: Plan created (this document)
2. → AI Panel critique (MANDATORY)
3. → Human approval
4. → M4 Problem #31: RED → GREEN → COMMIT
5. → M4 Problem #32: RED → GREEN → COMMIT
6. → M5: Final validation, coverage, memory update
7. → Notify orchestrator
