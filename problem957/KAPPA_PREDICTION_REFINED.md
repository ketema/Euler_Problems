# Refined κ_t Prediction Models

**Date**: 2025-11-09
**Goal**: Develop predictive model for κ_t using geometric features
**Status**: Partial success (Days 1-5), needs refinement for Days 6+

---

## Summary

We tested 3 models to predict κ_t from geometric features:

1. **Hilbert gap only**: κ_t ≈ a · (gap)^b
2. **Point count only**: κ_t ≈ a · (m)^b
3. **Combined**: κ_t ≈ a · (gap)^b · (m)^c

**Best Model**: Combined (20.3% average error on Days 1-5)

```
κ_t ≈ 4.836×10⁻¹¹ · (gap)^1.25 · (m)^0.0
```

**Surprising Finding**: Point count exponent ≈ 0 (doesn't matter!)

---

## Model Performance

### Training Data (Days 1-5)

| Model | Avg Error | Best Feature |
|-------|-----------|-------------|
| Hilbert gap | 20.9% | Spatial density |
| Point count | 34.3% | Combinatorics |
| **Combined** | **20.3%** | **Hybrid** |

### Detailed Predictions (Combined Model)

| Day | Predicted κ_t | Actual κ_t | Error |
|-----|--------------|-----------|-------|
| 1 | 1.000000 | 1.000000 | 0.0% ✓ |
| 2 | 0.188642 | 0.123000 | 53.4% ~ |
| 3 | 0.059279 | 0.074000 | 19.9% ✓ |
| 4 | 0.013256 | 0.014800 | 10.4% ✓ |
| 5 | 0.003227 | 0.002740 | 17.8% ✓ |

**Performance**: 4/5 days < 20% error, 1 day moderate error

---

## Critical Discovery: Extrapolation Failure

### Prediction for Days 6-10

| Day | Predicted | Actual (OEIS) | Error |
|-----|-----------|--------------|-------|
| 6 | 7.86×10⁻⁴ | 2.22×10⁻⁴ | 254% ✗ |
| 7 | 1.91×10⁻⁴ | 1.89×10⁻⁵ | 912% ✗ |
| 8 | 4.66×10⁻⁵ | 1.37×10⁻⁶ | 3299% ✗ |
| 9 | 1.13×10⁻⁵ | 8.56×10⁻⁸ | 13148% ✗ |
| 10 | 2.76×10⁻⁶ | 4.75×10⁻⁹ | 58020% ✗ |

**Catastrophic failure** for Days 6+!

### Why Extrapolation Fails

**Root Cause**: Hilbert gap extrapolation is wrong

We used: `gap(t) ≈ 517M · exp(-1.13·t)` (exponential decay)

**Problem**:
- Fitted exponent is **+1.25** (direct relationship)
- Formula predicts: smaller gap → smaller κ
- Reality: smaller gap → **larger** coincidence → **smaller** κ

**This is backwards!**

### The Issue

**Correct relationship** should be:
```
κ_t ∝ (1/gap)^α  (inverse relationship)
```

But our fit gives:
```
κ_t ∝ (gap)^1.25  (direct relationship)  ← WRONG!
```

**Why this happened**:
- Days 1-5: gap decreases, κ decreases → appears correlated
- But underlying mechanism is: density ↑ → coincidence ↑ → κ ↓
- Need to fit INVERSE or use different functional form

---

## Corrected Model

### Approach 1: Inverse Hilbert Gap

Try: `κ_t = a · (1/gap)^b · (m)^c`

**Expectation**: Higher density (1/gap ↑) → lower κ

### Approach 2: Density-Based

Define **spatial density** = 1 / (Hilbert gap)

```
density_t = 1 / gap_t

κ_t = a · (density_t)^b · (m)^c
```

**Physical interpretation**:
- Higher density → more points per unit Hilbert length
- More overlap → more coincidence
- Lower κ_t

### Approach 3: Regime Change

Model might have **two regimes**:

**Regime 1 (Days 1-5)**: Geometric structure dominates
- Gap matters
- Formula: κ_t ≈ f(gap, m)

**Regime 2 (Days 6+)**: Combinatorial explosion dominates
- Pure counting overwhelms geometry
- Formula: κ_t ≈ g(m, P_t)

**Evidence**: Error jumps at Day 6 (254% vs 18% at Day 5)

---

## What Works

### Hilbert Gap Correlation (Days 1-5)

**Finding**: Hilbert gap predicts κ_t with 20% accuracy

**Formula**:
```
κ_t ≈ 5.69×10⁻¹¹ · (gap)^1.24
```

**Limitations**:
- Only valid for Days 1-5
- Breaks down at Day 6+
- Requires correct extrapolation of gap

### Point Count is Secondary

**Surprising result**: m exponent ≈ 0

**Interpretation**:
- Point count alone doesn't predict κ well
- Spatial distribution (gap) more important
- Matches our Hilbert locality finding

---

## What Doesn't Work

### Exponential Gap Extrapolation

**Assumption**: gap(t) = A · exp(B·t)

**Fitted**: gap(t) ≈ 517M · exp(-1.13·t)

**Problem**: Assumes exponential decay continues forever

**Reality**: Gap likely reaches floor or changes behavior

### Direct Power Law

**Model**: κ_t = a · (gap)^b with b > 0

**Issue**: Predicts κ ↑ when gap ↑, but reality is opposite

**Fix**: Use inverse: κ_t = a · (gap)^b with b < 0

---

## Recommended Fix

### Step 1: Use Inverse Relationship

**New model**: `κ_t = a · (gap)^b` where **b < 0**

**Constraint**: Force b to be negative in fitting

### Step 2: Better Gap Extrapolation

**Option A**: Power law decay
```
gap(t) = A · t^(-α)  (hyperbolic decay)
```

**Option B**: Piecewise model
```
gap(t) = {
  exponential decay   for t ≤ 5
  power law decay     for t > 5
}
```

**Option C**: Compute actual gaps (no extrapolation)
- Run Hilbert analysis for Days 6-10
- Use real data instead of estimates

### Step 3: Incorporate d_t

Once we know variety degree:
```
κ_t = a · (1/gap)^b · (1/d_t)^c · (m)^e
```

**Rationale**: Higher degree → more coincidence → lower κ

---

## Theoretical Insight

### Why Hilbert Gap Matters

**Spatial density hypothesis**:
```
density = 1 / gap
κ_t ∝ (1 / density)^α
κ_t ∝ (gap)^α  where α > 0
```

**Mechanism**:
1. Points densify in Hilbert space (gap ↓)
2. More points in same region → spatial overlap
3. Lines through overlapping points coincide
4. Coincidence ↑ → κ_t ↓

### Why Point Count Alone Fails

**Point count**: m_t measures **quantity**

**Hilbert gap**: measures **distribution**

**Key insight**: κ_t depends on HOW points are distributed, not just how many

**Example**:
- 1000 points spread uniformly: low coincidence
- 1000 points clustered tightly: high coincidence
- Same m, different κ!

---

## Next Steps

### Immediate (Compute Real Gaps)

1. Run Hilbert analysis for Day 6 (if feasible)
2. Compare predicted vs actual gap
3. Test if gap extrapolation was wrong

### Short-Term (Fix Model)

1. Refit with inverse gap: κ_t = a · (1/gap)^b
2. Test power-law decay: gap(t) = A · t^(-α)
3. Validate on Days 1-5, extrapolate to 6-10

### Medium-Term (Incorporate Variety Degree)

1. Compute d_t for Days 2-5 (polynomial fitting)
2. Add d_t to model: κ_t = f(gap, m, d_t)
3. Test if degree improves prediction

### Long-Term (Regime Model)

1. Identify regime boundary (Day 5/6?)
2. Fit separate models for each regime
3. Predict transition point from theory

---

## Conclusion

**Success**: 20% error on Days 1-5 using Hilbert gap

**Failure**: Catastrophic extrapolation error for Days 6+

**Root Cause**:
- Incorrect functional form (direct vs inverse)
- Wrong gap extrapolation (exponential vs reality)

**Fix**:
- Use inverse relationship: κ_t ∝ (1/gap)^α
- Better gap extrapolation or real computation
- Consider regime change at Day 6

**Key Insight**:
Spatial density (Hilbert gap) is the **primary driver** of κ_t, not point count. This validates the locality preservation hypothesis.

---

## Files

- `quick_kappa_refinement.py`: Model fitting script
- `refine_kappa_prediction.py`: Full analysis (slow, computing Days 4-5)

---

**Date**: 2025-11-09
**Conclusion**: Model works for Days 1-5, needs inverse form and better extrapolation for Days 6+
