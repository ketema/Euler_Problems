#!/usr/bin/env python3
"""
PROMPT 3: OEIS Cross-Reference and Verification
DETERMINISTIC execution using oeis library

Objective: Search OEIS database for sequence [2, 8, 28, 184, 1644]
Tools: oeis library for sequence lookup
Output: Matching sequences, formulas, and insights

Context: PROMPT 2 produced g(16)=1,973,818 via polynomial extrapolation,
but this was ALREADY REJECTED by Project Euler. Need to find if there's
a known sequence pattern we're missing.
"""

import json
from typing import List, Dict, Optional

# Verified sequence from PROMPT 1
VERIFIED_SEQUENCE = [2, 8, 28, 184, 1644]

def print_section(title: str):
    """Print formatted section header"""
    print("=" * 80)
    print(title)
    print("=" * 80)
    print()

def search_oeis_sequence() -> Optional[Dict]:
    """Search OEIS for exact sequence match"""
    print_section("STEP 1: OEIS EXACT SEQUENCE SEARCH")

    try:
        # Import oeis library
        from oeis import oeis

        print(f"Searching OEIS for sequence: {VERIFIED_SEQUENCE}")
        print()

        # Search for the sequence
        results = oeis(VERIFIED_SEQUENCE)

        if not results:
            print("✗ No exact matches found in OEIS")
            print()
            return None

        print(f"✓ Found {len(results)} matching sequence(s):")
        print()

        matches = []
        for i, result in enumerate(results[:5]):  # Limit to top 5
            print(f"Match {i+1}: {result.id}")
            print(f"  Name: {result.name}")

            # Get sequence values
            if hasattr(result, 'sequence'):
                print(f"  Sequence: {result.sequence[:10]}")

            # Get formula if available
            if hasattr(result, 'formulas') and result.formulas:
                print(f"  Formulas:")
                for formula in result.formulas[:3]:  # Top 3 formulas
                    print(f"    - {formula}")

            # Get comments if available
            if hasattr(result, 'comments') and result.comments:
                print(f"  Comments:")
                for comment in result.comments[:2]:  # Top 2 comments
                    print(f"    - {comment}")

            # Get OEIS URL
            url = f"https://oeis.org/{result.id}"
            print(f"  URL: {url}")
            print()

            matches.append({
                "id": str(result.id),
                "name": str(result.name) if hasattr(result, 'name') else "",
                "url": url,
                "formulas": [str(f) for f in result.formulas[:3]] if hasattr(result, 'formulas') and result.formulas else [],
                "sequence": [int(v) for v in result.sequence[:20]] if hasattr(result, 'sequence') else []
            })

        return {"matches": matches}

    except ImportError:
        print("✗ OEIS library not available")
        print("  Install with: poetry add oeis")
        print()
        return None
    except Exception as e:
        print(f"✗ Error searching OEIS: {e}")
        print()
        return None

def search_oeis_subsequences() -> Optional[Dict]:
    """Search OEIS for partial matches"""
    print_section("STEP 2: OEIS SUBSEQUENCE SEARCH")

    try:
        from oeis import oeis

        # Try different subsequences
        subsequences = [
            ([2, 8, 28], "First 3 terms"),
            ([8, 28, 184], "Middle 3 terms"),
            ([28, 184, 1644], "Last 3 terms"),
            ([2, 8, 28, 184], "First 4 terms")
        ]

        all_matches = {}

        for subseq, description in subsequences:
            print(f"Searching for {description}: {subseq}")

            try:
                results = oeis(subseq)

                if results:
                    print(f"  ✓ Found {len(results)} match(es)")
                    # Just note first match
                    if len(results) > 0:
                        first = results[0]
                        print(f"    Top match: {first.id} - {first.name if hasattr(first, 'name') else ''}")

                        all_matches[description] = {
                            "id": str(first.id),
                            "name": str(first.name) if hasattr(first, 'name') else "",
                            "url": f"https://oeis.org/{first.id}"
                        }
                else:
                    print(f"  ✗ No matches")
            except Exception as e:
                print(f"  ✗ Error: {e}")

            print()

        return {"subsequence_matches": all_matches} if all_matches else None

    except ImportError:
        print("✗ OEIS library not available")
        return None
    except Exception as e:
        print(f"✗ Error: {e}")
        return None

def analyze_growth_pattern() -> Dict:
    """Analyze growth pattern for known sequence types"""
    print_section("STEP 3: GROWTH PATTERN ANALYSIS")

    print("Analyzing growth characteristics:")
    print()

    # Compute ratios
    ratios = [VERIFIED_SEQUENCE[i+1] / VERIFIED_SEQUENCE[i] for i in range(len(VERIFIED_SEQUENCE)-1)]
    print(f"Growth ratios: {[f'{r:.3f}' for r in ratios]}")

    # Check if superexponential
    ratio_growth = [ratios[i+1] / ratios[i] for i in range(len(ratios)-1)]
    print(f"Ratio of ratios: {[f'{r:.3f}' for r in ratio_growth]}")
    print()

    # Classify
    if all(r > 1 for r in ratio_growth):
        print("Pattern: SUPEREXPONENTIAL (ratio of ratios > 1)")
        print("  → Growth accelerates faster than geometric")
        print("  → Typical of: factorial-like, double exponential, combinatorial explosion")
    elif all(abs(r - 1) < 0.1 for r in ratio_growth):
        print("Pattern: EXPONENTIAL (ratio of ratios ≈ 1)")
        print("  → Constant growth ratio")
    else:
        print("Pattern: MIXED (ratio of ratios varies)")
        print("  → May be polynomial, sub-exponential, or special function")

    print()

    # Check for factorial-like growth
    import math
    print("Checking factorial-like patterns:")
    for i, val in enumerate(VERIFIED_SEQUENCE):
        # Check if val ≈ n! or related
        if i > 0:
            factorial_ratio = val / math.factorial(i)
            print(f"  g({i}) / {i}! = {factorial_ratio:.2f}")

    print()

    return {
        "ratios": [float(r) for r in ratios],
        "ratio_growth": [float(r) for r in ratio_growth],
        "classification": "superexponential" if all(r > 1 for r in ratio_growth) else "other"
    }

def main():
    print_section("PROMPT 3: OEIS VERIFICATION AND ANALYSIS")

    print("Input sequence from PROMPT 1:")
    print(f"  {VERIFIED_SEQUENCE}")
    print()

    print("Context:")
    print("  PROMPT 2 polynomial extrapolation gave g(16) = 1,973,818")
    print("  This answer was ALREADY REJECTED by Project Euler")
    print("  Need to find if this sequence has known non-polynomial formula")
    print()

    # Run searches
    exact_match = search_oeis_sequence()
    subsequence_matches = search_oeis_subsequences()
    growth_analysis = analyze_growth_pattern()

    # Compile results
    print_section("SUMMARY AND CONCLUSIONS")

    results = {
        "sequence": VERIFIED_SEQUENCE,
        "oeis_exact_match": exact_match,
        "oeis_subsequence_matches": subsequence_matches,
        "growth_analysis": growth_analysis
    }

    if exact_match and exact_match.get("matches"):
        print("✓ OEIS MATCH FOUND")
        print()
        print("Action items:")
        print("  1. Review OEIS formulas for alternative g(16) computation")
        print("  2. Check if OEIS sequence continues differently than polynomial")
        print("  3. Look for special properties or constraints in OEIS comments")
        print()

        for match in exact_match["matches"][:3]:
            print(f"  Check {match['id']}: {match['url']}")

        print()
    else:
        print("✗ NO OEIS MATCH")
        print()
        print("This means one of two things:")
        print("  1. The sequence is novel/specific to this problem")
        print("  2. We're not simulating the problem correctly")
        print()
        print("Recommendations:")
        print("  A. Re-examine problem statement for hidden constraints")
        print("  B. Test alternative interpretations of 'maximal possible'")
        print("  C. Consider that g(1)=8, g(2)=28 are examples, not universals")
        print()

    # Output JSON
    print("JSON output for next steps:")
    print(json.dumps(results, indent=2))
    print()

    # Decision point
    if exact_match and exact_match.get("matches"):
        print("NEXT STEP: Review OEIS formulas, then proceed to PROMPT 4 if alternative formula found")
    else:
        print("NEXT STEP: Return to problem interpretation analysis (not PROMPT 4)")

    print()

if __name__ == "__main__":
    main()
