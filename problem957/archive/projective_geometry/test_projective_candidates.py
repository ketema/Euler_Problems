#!/usr/bin/env python3
"""
Test projective plane candidates for g(16)

INSIGHT: Rejected 1893 = PG(2,43) and 2257 = PG(2,47) suggests connection
to projective geometry, but EXACT formula q²+q+1 failed.

Test nearby values and variations:
- PG(2,q) ± small adjustments
- Different prime powers
- Potentially g(16) = PG(2,q) - k for some small k
"""

def pg(q):
    """Projective plane formula: q²+q+1"""
    return q**2 + q + 1

def main():
    print("="*80)
    print("PROJECTIVE PLANE CANDIDATES FOR g(16)")
    print("="*80)
    print()

    # Rejected answers
    rejected = {
        4, 14, 16, 20, 56, 74, 112, 152, 256, 308, 828,
        1778, 1893, 2257,
        1973818, 633250439,  # Large ones
    }

    print("Testing candidates based on projective plane formula:")
    print()

    # Test exact PG(2,q) for prime powers
    prime_powers = [
        2, 3, 4, 5, 7, 8, 9, 11, 13, 16, 17, 19, 23, 25, 27, 29, 31, 32,
        37, 41, 43, 47, 49, 53, 59, 61, 64, 67, 71, 73, 79, 81, 83, 89, 97
    ]

    candidates = []

    for q in prime_powers:
        val = pg(q)
        if val not in rejected and val < 10000:
            candidates.append((val, f"PG(2,{q})", q))

    # Sort by value
    candidates.sort()

    print("EXACT PG(2,q) VALUES (not yet rejected):")
    print()
    for val, desc, q in candidates[:20]:  # Show first 20
        print(f"  {val:5d} = {desc}")

    print()
    print("="*80)
    print("VARIATIONS:")
    print("="*80)
    print()

    # Test PG(2,q) - 3 (since we start with 3 reds)
    print("PG(2,q) - 3 (subtract 3 red points):")
    print()
    variations = []
    for q in prime_powers:
        val = pg(q) - 3
        if val not in rejected and 100 < val < 10000:
            variations.append((val, f"PG(2,{q})-3", q))

    variations.sort()
    for val, desc, q in variations[:15]:
        print(f"  {val:5d} = {desc} = {pg(q)}-3")

    print()

    # Test PG(2,q) - 2 (since we start with 2 blues)
    print("PG(2,q) - 2 (subtract 2 initial blues):")
    print()
    variations2 = []
    for q in prime_powers:
        val = pg(q) - 2
        if val not in rejected and 100 < val < 10000:
            variations2.append((val, f"PG(2,{q})-2", q))

    variations2.sort()
    for val, desc, q in variations2[:15]:
        print(f"  {val:5d} = {desc} = {pg(q)}-2")

    print()

    # Note: 307 is very close to rejected 308
    print("="*80)
    print("NOTABLE OBSERVATIONS:")
    print("="*80)
    print()
    print("1. PG(2,17) = 307, but 308 was rejected (linear extrapolation)")
    print("   → Try 307?")
    print()
    print("2. PG(2,41) = 1723, close to rejected 1778 (quadratic extrapolation)")
    print("   → Try 1723?")
    print()
    print("3. Exact PG(2,43)=1893 and PG(2,47)=2257 both rejected")
    print("   → Answer is NOT exact q²+q+1 formula")
    print()
    print("4. Maybe answer is PG(2,q) - k for small k?")
    print("   → Test variations above")
    print()

    print("="*80)
    print("TOP CANDIDATES TO TEST (in priority order):")
    print("="*80)
    print()

    top_candidates = [
        (307, "PG(2,17) - very close to rejected 308"),
        (1723, "PG(2,41) - near rejected 1778"),
        (2451, "PG(2,49) - between rejected 2257 and next"),
        (1720, "PG(2,41)-3 - subtract 3 reds"),
        (304, "PG(2,17)-3"),
        (1891, "PG(2,43)-2 - subtract 2 blues"),
    ]

    for i, (val, reason) in enumerate(top_candidates, 1):
        print(f"{i}. {val:5d} - {reason}")

    print()

if __name__ == "__main__":
    main()
