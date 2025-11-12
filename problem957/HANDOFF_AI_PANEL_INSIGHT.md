# Problem 957: AI Panel Critical Insight

**Date**: 2025-11-12
**Token Usage**: 110K/200K (55%)
**Status**: Breakthrough insight from AI Panel - geometric equivalence discovered

---

## CRITICAL DISCOVERY

Two DIFFERENT initial blue configurations produce IDENTICAL sequences:

1. **Config A**: B1=(1,1), B2=(3,9) → g(1)=8, g(2)=28
2. **Config B**: B1=(2,3), B2=(5,7) → g(1)=8, g(2)=28

**AI Panel Assessment**:
> "Two configs with g(1)=g(2) means they're in the same equivalence class. This is your key insight."

---

## WHAT WE KNOW

### Verified Facts
- Reds (fixed): R1=(0,0), R2=(1,0), R3=(0,1)
- Both configs give: g(1)=8, g(2)=28, g(3)=184, g(4)=1644, g(5)=19068
- Other random configs tested give LESS (e.g., g(1)=7 or g(1)=4)
- g(6) computation times out after 5+ minutes
- Problem solvable in 74 minutes (PE leaderboard data)

### Properties Tested
- ✗ NOT same slope: Config A has slope 4, Config B has slope 4/3
- ✗ NOT projectively equivalent by simple transformation
- ✗ NOT collinear with reds
- ? UNKNOWN geometric invariant connects them

---

## CURRENT HYPOTHESIS

There exists a **geometric invariant** (not slope, not simple affine transformation) that:
1. Is shared by (1,1),(3,9) and (2,3),(5,7)
2. Determines the maximum g(n) achievable
3. Enables fast computation of g(16) without simulating all days

**Candidates for this invariant**:
- Cross-ratio involving reds + blues
- Projective coordinates / homogeneous representation
- Distance ratios from blues to reds
- Angle subtended by blues at origin
- Area ratios of triangles formed
- Combinatorial incidence structure

---

## AI PANEL RECOMMENDATIONS

**Priority 1 (CRITICAL)**: Investigate WHY (1,1),(3,9) ≡ (2,3),(5,7)
- Check projective transformation (full 3x3 matrix, not just 2x2)
- Compute cross-ratios using projective coordinates
- Check if determinants/invariants match
- Verify if blues are collinear with some red subset

**Priority 2 (CRITICAL)**: Incremental line generation optimization
- Current: O(n^4) checking ALL line pairs
- Optimized: Only check NEW lines against existing
- Could make g(6) tractable

**Priority 3**: Exploit geometric structure
- Research: Pappus theorem, Desargues theorem
- Configuration spaces of point-line arrangements
- Known results on pencil configurations (lines through fixed points)

---

## NEXT STEPS

1. **Compute projective invariants** for both configs
   - Convert to homogeneous coordinates [x:y:1]
   - Find projective transformation P such that P·Config A = Config B
   - If exists, this explains equivalence

2. **Test hypothesis systematically**
   - Generate many more configs
   - Group by g(1), g(2) values
   - Find invariant that separates equivalence classes

3. **Literature search**: "Pencil of lines" + "intersection points" + "projective geometry"
   - All lines pass through 3 fixed points (reds)
   - This is a SPECIAL configuration
   - Must be known results about this

---

## OPEN QUESTIONS

1. **Why do different configs give same g(n)?**
   - Is there a continuous family of equivalent configs?
   - Or discrete set of optimal configs?

2. **What determines optimality?**
   - Why is g(1)=8 the maximum, not 9 or 7?
   - What geometric constraint limits this?

3. **How to compute g(16) in 74 minutes?**
   - Can't be brute force (g(6) already times out)
   - Must exploit structure we're missing

---

## EVIDENCE

### Configuration Test Results
```
ORIGINAL CONFIG: B1=(1,1), B2=(3,9)
  g(1) = 8, g(2) = 28

Testing other configurations:
  B1=(2, 3), B2=(5, 7): g(1)=8 = 8, g(2)=28 = 28  ← SAME!
  B1=(1, 2), B2=(2, 1): g(1)=7 < 8, g(2)=19 < 28
  B1=(3, 3), B2=(5, 5): g(1)=4 < 8, g(2)=8 < 28
  B1=(1, 1), B2=(2, 3): g(1)=7 < 8, g(2)=23 < 28
```

### AI Panel Code Review
- Algorithm fundamentally correct
- O(n^4) complexity makes g(6) intractable
- Missing geometric insight prevents optimization
- "Random sampling misses systematic structure"

---

## TOKEN BUDGET WARNING

**Current**: 110K/200K (55%)
**Remaining**: 90K tokens

**Recommendation**:
- Prepare for handoff if projective geometry investigation doesn't yield breakthrough
- Document all invariant tests attempted
- List all geometric properties checked

---

## REFERENCES

- Project Euler Problem 957: https://projecteuler.net/problem=957
- AI Panel feedback: critique_code tool output
- Leaderboard: 74-minute solve time (fastest)
- Git history: 75+ hours previous work, 50+ rejected answers
