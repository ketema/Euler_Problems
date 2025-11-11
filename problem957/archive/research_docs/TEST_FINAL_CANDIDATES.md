# Final Candidate Testing Plan

Based on AI Panel feedback and inverse analysis, test in this order:

## Priority Order

1. **4** - GCD(8,28), fundamental divisor, inverse of LCM=56
2. **20** - Difference 28-8, inverse of sum operations
3. **16** - n itself, √256, self-referential
4. **2** - Initial blues (problem parameter)
5. **3** - Initial reds (problem parameter)
6. **5** - Total initial (3+2)
7. **6** - Initial lines (3×2)
8. **36** - Simple sum 8+28
9. **74** - POINT word sum
10. **14** - 112/8

## Testing Status

- [x] 4 ✗ REJECTED
- [x] 14 ✗ REJECTED
- [x] 20 ✗ REJECTED
- [x] 16 ✗ REJECTED (self-referential failed)
- [ ] 36, 74, 2, 3, 5, 6 (Remaining simple)

**PIVOT: Return to simulation validation**

## Rejection Log

1. **4** - GCD(8,28) rejected (2025-01-11)
   - Reasoning: Inverse of rejected LCM=56
   - Mathematical justification: Fundamental divisor

2. **14** - 112/8 or 2×7 rejected (2025-01-11)
   - Reasoning: Related to rejected 112 (P×G product)
   - Also: 28/2 = 14

3. **20** - 28-8 rejected (2025-01-11) ⚠️ HIGH PRIORITY FAILURE
   - Reasoning: Difference between g(1) and g(2), growth rate
   - Mathematical: Inverse of sum operations
