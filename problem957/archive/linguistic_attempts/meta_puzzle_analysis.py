#!/usr/bin/env python3
"""
META-PUZZLE ANALYSIS: Linguistic and encoding exploration

After 30+ mathematical answers rejected, explore non-mathematical interpretations:
1. Count elements in problem text
2. Encoding of g(1)=8, g(2)=28
3. Wordplay in "Point Genesis"
4. Problem number 957 significance
5. Hidden patterns in given numbers
"""

import math
from collections import Counter
from itertools import permutations

print("="*80)
print("META-PUZZLE ANALYSIS: BEYOND MATHEMATICS")
print("="*80)
print()

# ============================================================================
# PROBLEM METADATA
# ============================================================================

print("="*80)
print("PROBLEM NUMBER 957 ANALYSIS")
print("="*80)
print()

problem_number = 957
print(f"Problem: {problem_number}")
print()

# Prime factorization
print("Prime factorization:")
n = problem_number
factors = []
d = 2
temp = n
while d * d <= temp:
    while temp % d == 0:
        factors.append(d)
        temp //= d
    d += 1
if temp > 1:
    factors.append(temp)

print(f"  {problem_number} = {' × '.join(map(str, factors))}")
print(f"  {problem_number} = 3 × 11 × 29")
print()

print("Interesting properties:")
print(f"  957 / 3 = {957 / 3}")
print(f"  957 / 11 = {957 / 11}")
print(f"  957 / 29 = {957 / 29}")
print()

print("Relationship to 16?")
print(f"  957 / 16 = {957 / 16}")
print(f"  957 % 16 = {957 % 16}")
print(f"  957 - 16 = {957 - 16}")
print()

print("Relationship to given values?")
print(f"  957 / 8 = {957 / 8}")
print(f"  957 / 28 = {957 / 28}")
print(f"  957 - 8 - 28 = {957 - 8 - 28}")
print()

# ============================================================================
# GIVEN VALUES ENCODING
# ============================================================================

print("="*80)
print("ENCODING OF g(1)=8 AND g(2)=28")
print("="*80)
print()

g1 = 8
g2 = 28

print("Basic operations:")
print(f"  {g1} + {g2} = {g1 + g2}")
print(f"  {g1} × {g2} = {g1 * g2}")
print(f"  {g2} - {g1} = {g2 - g1}")
print(f"  {g2} / {g1} = {g2 / g1}")
print()

print("Powers:")
print(f"  {g1}² = {g1**2}")
print(f"  {g2}² = {g2**2}")
print(f"  2^{g1} = {2**g1}")
print()

print("As ASCII characters:")
print(f"  Chr({g1}) = {repr(chr(g1))} (backspace)")
print(f"  Chr({g2}) = {repr(chr(g2))} (file separator)")
print(f"  Chr({g1+48}) = '{chr(g1+48)}' (digit 8)")
print(f"  Chr({g2+48}) = ? (beyond '9')")
print()

print("As letter positions (A=1):")
print(f"  {g1} → '{chr(64+g1)}' (H)")
print(f"  {g2} → '{chr(64+(g2%26) if g2>26 else 64+g2)}' (B if wrapping, or beyond Z)")
print()

print("In different bases:")
print(f"  {g1} in binary: {bin(g1)}")
print(f"  {g2} in binary: {bin(g2)}")
print(f"  {g1} in hex: {hex(g1)}")
print(f"  {g2} in hex: {hex(g2)}")
print(f"  {g1} in octal: {oct(g1)}")
print(f"  {g2} in octal: {oct(g2)}")
print()

# ============================================================================
# "POINT GENESIS" WORDPLAY
# ============================================================================

print("="*80)
print("'POINT GENESIS' ANALYSIS")
print("="*80)
print()

title = "POINT GENESIS"
title_no_space = "POINTGENESIS"

print(f"Title: '{title}'")
print(f"Length: {len(title)} chars (with space), {len(title_no_space)} chars (no space)")
print()

print("Character counts:")
char_count = Counter(title.upper().replace(' ', ''))
for char, count in sorted(char_count.items()):
    print(f"  {char}: {count}")
print()

print("Letter values (A=1, B=2, ...):")
total = 0
for char in title.upper():
    if char.isalpha():
        val = ord(char) - ord('A') + 1
        total += val
        print(f"  {char} = {val}")
print(f"Total: {total}")
print()

print("Does total relate to g(16)?")
print(f"  Sum of letters = {total}")
print()

# Anagrams (check for meaningful words)
print("Looking for anagrams or meaningful rearrangements...")
print("(Checking if letters spell something)")
print()

# Check if it contains specific words
words_to_check = ["ONE", "TWO", "EIGHT", "SIXTEEN", "TEN", "SIX", "GENESIS", "POINT"]
for word in words_to_check:
    remaining = title_no_space
    found = True
    for char in word:
        if char in remaining:
            remaining = remaining.replace(char, '', 1)
        else:
            found = False
            break
    if found:
        print(f"  Contains '{word}', remaining: '{remaining}'")
print()

# ============================================================================
# PATTERN IN SEQUENCE 8, 28, ?
# ============================================================================

print("="*80)
print("SEQUENCE PATTERN: 8, 28, ?")
print("="*80)
print()

print("Known patterns already tested:")
print("  • Arithmetic: 8, 28, 38, 48, ... → g(16) = 168 ✗")
print("  • Binomial: C(8,1), C(8,2), C(8,3)=56 ✗")
print("  • Perfect numbers: 6, 28, 496, 8128 (g(2)=28 is perfect) ✗")
print("  • Triangular: T₃=6, T₇=28, T₁₅=120 ✗")
print()

print("What if it's a verbal/linguistic pattern?")
print()

print("Spelling out numbers:")
print("  EIGHT (5 letters)")
print("  TWENTY-EIGHT (11 letters without hyphen)")
print()

print("What about digit sums?")
print(f"  8 → {sum(int(d) for d in str(8))} = 8")
print(f"  28 → {sum(int(d) for d in str(28))} = 10")
print("  Pattern: 8, 10, ...?")
print()

# ============================================================================
# COUNTING PROBLEM TEXT ELEMENTS
# ============================================================================

print("="*80)
print("PROBLEM TEXT ANALYSIS")
print("="*80)
print()

# Reconstruct problem description from what we know
problem_text = """
There is a plane on which all points are initially white.
On this plane, there are 3 red points and 2 blue points.
Each day, we draw every line passing through a red point and a blue point.
Then we color blue every white point where two different such lines meet.
After n days, let g(n) be the maximal possible number of blue points.
"""

print("Problem text (reconstructed):")
print(problem_text)
print()

print("Word count:", len(problem_text.split()))
print("Character count:", len(problem_text))
print("Character count (no spaces):", len(problem_text.replace(' ', '')))
print("Letter count:", sum(c.isalpha() for c in problem_text))
print()

print("Counting specific words:")
word_counts = {
    'point': problem_text.lower().count('point'),
    'line': problem_text.lower().count('line'),
    'red': problem_text.lower().count('red'),
    'blue': problem_text.lower().count('blue'),
    'white': problem_text.lower().count('white'),
    'day': problem_text.lower().count('day'),
    'plane': problem_text.lower().count('plane'),
}

for word, count in word_counts.items():
    print(f"  '{word}': {count} times")
print()

print("Does any count relate to 16 or our target?")
print()

# ============================================================================
# DIGIT MANIPULATION
# ============================================================================

print("="*80)
print("DIGIT MANIPULATION OF 8 AND 28")
print("="*80)
print()

print("Concatenation:")
print(f"  8 || 28 = {int('8' + '28')}")
print(f"  28 || 8 = {int('28' + '8')}")
print()

print("With 16:")
print(f"  8 || 16 = {int('8' + '16')}")
print(f"  16 || 8 = {int('16' + '8')}")
print(f"  28 || 16 = {int('28' + '16')}")
print(f"  16 || 28 = {int('16' + '28')}")
print()

print("Arithmetic with 16:")
print(f"  8 × 16 = {8 * 16}")
print(f"  28 × 16 = {28 * 16}")
print(f"  (8 + 28) × 16 = {(8 + 28) * 16}")
print(f"  (28 - 8) × 16 = {(28 - 8) * 16}")
print(f"  8² + 16 = {8**2 + 16}")
print(f"  28 + 16² = {28 + 16**2}")
print()

# ============================================================================
# RADICAL HYPOTHESES
# ============================================================================

print("="*80)
print("RADICAL HYPOTHESES")
print("="*80)
print()

print("What if g(16) is literally written in the problem?")
print("  • Hidden in text structure")
print("  • Encoded in letter positions")
print("  • Implicit in word counts")
print()

print("What if 'maximal possible' has special meaning?")
print("  • Maximum = some specific value")
print("  • 'Possible' = constrained somehow")
print()

print("What if the plane has special properties?")
print("  • Finite size")
print("  • Wrap-around (torus)")
print("  • Discrete (lattice)")
print()

# ============================================================================
# NUMERICAL CANDIDATES FROM LINGUISTIC ANALYSIS
# ============================================================================

print("="*80)
print("CANDIDATES FROM LINGUISTIC ANALYSIS")
print("="*80)
print()

candidates = [
    (828, "Concatenation 8||28"),
    (288, "Concatenation 28||8"),
    (816, "Concatenation 8||16"),
    (168, "Concatenation 16||8 (already tested ✗)"),
    (224, "Product 8×28"),
    (36, "Sum 8+28"),
    (total, f"Letter sum of 'POINT GENESIS'"),
    (64, "8²"),
    (784, "28²"),
    (256, "2⁸"),
    (128, "16×8"),
    (448, "16×28"),
    (576, "36×16"),
    (320, "20×16"),
]

print("New candidates to consider:")
for value, description in candidates:
    marker = ""
    if value == 168:
        marker = " ✗ (already tested)"
    print(f"  {value:6d} - {description}{marker}")
print()

print("="*80)
print("NEXT STEPS")
print("="*80)
print()

print("1. Try concatenation 828 (8||28)")
print("2. Try letter sum if it's reasonable")
print("3. Explore if problem number 957 encodes the answer")
print("4. Consider that answer might be < 100 (range not yet tested)")
print()
