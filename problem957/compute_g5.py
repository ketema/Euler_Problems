#!/usr/bin/env python3
"""
COMPUTE g(5) - CRITICAL TEST FOR SATURATION HYPOTHESIS

This will compute g(5) to test if the sequence:
1. Saturates (plateaus) → suggests g(16) may be computable
2. Matches one of our three formulas → validates that formula
3. Continues exponential growth → suggests different approach needed

Expected time: 10 minutes to 2 hours depending on saturation
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from typing import Set, List
import time as time_module
import sys

def compute_day_exact(reds: List[Point], blues: Set[Point]) -> Set[Point]:
    """Compute next day's blues with exact arithmetic"""
    lines = []
    for red in reds:
        for blue in blues:
            if red != blue:
                lines.append(Line(red, blue))

    existing = set(reds).union(blues)
    new_intersections = set()

    for i, line1 in enumerate(lines):
        for line2 in lines[i+1:]:
            result = intersection(line1, line2)
            if result and hasattr(result[0], 'x'):
                p = result[0]
                if p not in existing:
                    new_intersections.add(p)

    return blues.union(new_intersections)

# Verified configuration that produces [2,8,28,184,1644]
reds = [
    Point(Rational(0), Rational(0)),
    Point(Rational(4), Rational(0)),
    Point(Rational(2), Rational(3))
]

blues = {
    Point(Rational(1), Rational(1)),
    Point(Rational(3), Rational(2))
}

print("="*80)
print("COMPUTING g(5) - CRITICAL TEST")
print("="*80)
print()
print("Verified sequence so far: [2, 8, 28, 184, 1644]")
print()

sequence = [len(blues)]
total_start = time_module.time()

for day in range(7):  # Try up to day 7
    day_start = time_module.time()

    print(f"Computing day {day} → {day+1}...")
    sys.stdout.flush()

    blues = compute_day_exact(reds, blues)

    day_elapsed = time_module.time() - day_start
    g_n = len(blues)
    sequence.append(g_n)

    print(f"  g({day+1}) = {g_n:>8,} (time: {day_elapsed:>8.1f}s)")
    sys.stdout.flush()

    # Stop if taking too long
    if day_elapsed > 1800:  # 30 minutes
        print(f"\n⚠️  Day {day}→{day+1} took {day_elapsed/60:.1f} minutes")
        print("   Stopping to avoid excessive computation")
        break

    # Stop if we have g(5)
    if day >= 4:
        print(f"\n✓ Successfully computed g(5) = {sequence[5]:,}")
        break

total_elapsed = time_module.time() - total_start

print()
print("="*80)
print("RESULTS")
print("="*80)
print()
print(f"Sequence: {sequence}")
print(f"Total time: {total_elapsed:.1f}s ({total_elapsed/60:.1f} minutes)")
print()

if len(sequence) > 5:
    print("ANALYSIS:")
    print(f"  g(0) = {sequence[0]}")
    print(f"  g(1) = {sequence[1]}")
    print(f"  g(2) = {sequence[2]}")
    print(f"  g(3) = {sequence[3]}")
    print(f"  g(4) = {sequence[4]:,}")
    print(f"  g(5) = {sequence[5]:,} ← NEW VALUE")
    print()

    # Ratios
    ratios = [sequence[i+1] / sequence[i] for i in range(len(sequence)-1)]
    print("  Growth ratios:")
    for i, r in enumerate(ratios):
        print(f"    g({i+1})/g({i}) = {r:.4f}")
    print()

    # Check for saturation
    if sequence[5] < sequence[4] * 2:
        print("  🔍 SATURATION DETECTED: Growth rate < 2×")
        print("     This suggests the sequence may be approaching a limit!")
    elif sequence[5] < sequence[4] * 5:
        print("  🔍 SLOWING GROWTH: Rate declining (< 5×)")
    else:
        print("  ⚠️  EXPONENTIAL GROWTH CONTINUES")

    # Test against our three formulas
    print()
    print("="*80)
    print("FORMULA VALIDATION")
    print("="*80)
    print()

    # Formula 1: g(n) = (-739/218 + 637/218*n)*g(n-1) + 456/109*g(n-2)
    from fractions import Fraction

    def formula1_predict_g5():
        g = sequence[:5]  # [2, 8, 28, 184, 1644]
        a0 = Fraction(-739, 218)
        a1 = Fraction(637, 218)
        b0 = Fraction(456, 109)
        n = 5
        return int(round(float((a0 + a1*n) * g[4] + b0 * g[3])))

    # Formula 2: g(n) = 187/25*g(n-1) + (-207/5 + 637/50*n)*g(n-2)
    def formula2_predict_g5():
        g = sequence[:5]
        a0 = Fraction(187, 25)
        b0 = Fraction(-207, 5)
        b1 = Fraction(637, 50)
        n = 5
        return int(round(float(a0 * g[4] + (b0 + b1*n) * g[3])))

    # Formula 3: g(n) = 705/67*g(n-1) - 608/67*g(n-2) - 2548/67
    def formula3_predict_g5():
        g = sequence[:5]
        a = Fraction(705, 67)
        b = Fraction(-608, 67)
        d = Fraction(-2548, 67)
        return int(round(float(a * g[4] + b * g[3] + d)))

    pred1 = formula1_predict_g5()
    pred2 = formula2_predict_g5()
    pred3 = formula3_predict_g5()
    actual = sequence[5]

    print(f"Formula 1 predicts g(5) = {pred1:,}")
    print(f"Formula 2 predicts g(5) = {pred2:,}")
    print(f"Formula 3 predicts g(5) = {pred3:,}")
    print(f"Actual g(5)            = {actual:,}")
    print()

    errors = [
        ("Formula 1", abs(pred1 - actual), 100 * abs(pred1 - actual) / actual),
        ("Formula 2", abs(pred2 - actual), 100 * abs(pred2 - actual) / actual),
        ("Formula 3", abs(pred3 - actual), 100 * abs(pred3 - actual) / actual)
    ]

    errors.sort(key=lambda x: x[1])

    print("Best match:")
    for name, error, pct in errors:
        status = "✓ MATCH" if pct < 1.0 else ("~ Close" if pct < 5.0 else "✗ Wrong")
        print(f"  {status} {name}: error = {error:,} ({pct:.2f}%)")

    if errors[0][2] < 1.0:
        print()
        print(f"⭐ {errors[0][0]} VALIDATED!")
        print("   This formula can be used to compute g(16)")

print()
print("="*80)
print("NEXT STEPS")
print("="*80)
print()
print("1. If saturation detected → g(16) may equal g(5) or grow slowly")
print("2. If formula validated → use it to compute g(16) directly")
print("3. If exponential continues → need different approach")
print()
