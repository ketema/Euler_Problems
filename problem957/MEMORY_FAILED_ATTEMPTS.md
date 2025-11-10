# Problem 957: Record of Failed Attempts

## CRITICAL: All Previous Answers Were WRONG

**Rejected by Project Euler:** g(16) = 1778

## Why 1778 Failed

### Attempt 1: Quadratic Formula (WRONG)
- Assumed: g(t) = 7t² - t + 2
- Gave: g(0)=2 ✓, g(1)=8 ✓, g(2)=28 ✓
- Predicted: g(16) = 7(256) - 16 + 2 = 1778
- **Result: REJECTED by Project Euler**

### Attempt 2: 5-Fan Misinterpretation (WRONG)
- Misread problem as having 5 red points instead of 3
- Led to same quadratic formula
- Answer: 1778 (also wrong)

### Attempt 3: Non-Quadratic Discovery
- Found configuration: reds at (0,0),(1,0),(0,1), blues at (-2,-2),(-2,-3/2)
- This gave: g(3) = 183 (NOT 62 as quadratic predicts!)
- Growth is **explosive, NOT quadratic**
- But simulation becomes intractable (>1600 points by day 4)

## The Correct Construction Rule

From problem statement: "every line passing through a red point and a blue point"

**CORRECT INTERPRETATION:**
- Draw ONLY lines connecting (one red) to (one blue)
- Do NOT draw red-red or blue-blue lines
- Day 0: 3 reds × 2 blues = 6 lines (not 10)

## What We Know For Certain

1. ✓ g(0) = 2 (given)
2. ✓ g(1) = 8 (given, verified with 6 lines → 15 pairs → 9 existing → 6 new)
3. ✓ g(2) = 28 (given)
4. ✗ g(t) is NOT quadratic (g(3) ≠ 62)
5. ✗ g(16) ≠ 1778 (rejected)

## Critical Insight from Git History

The growth rate depends HEAVILY on initial configuration:
- Some configurations give quadratic-like growth (but fail at g(3))
- Other configurations give explosive exponential growth
- The "maximal" configuration must be found

## What Remains Unknown

1. What is the correct initial configuration?
2. What is the actual growth pattern?
3. What is g(16)?

## Next Steps Required

1. ❌ Do NOT assume quadratic growth
2. ✓ Use exact rational geometry (SymPy)
3. ✓ Simulate multiple days to find actual pattern
4. ✓ Search for optimal initial configuration
5. ✓ Be prepared for non-polynomial growth
