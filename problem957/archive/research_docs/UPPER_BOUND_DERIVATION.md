# Upper Bound for m_{t+1} via Incidence Counting

## Problem Setup

**Given**: At day t, we have |B_t| = b blue points (and 3 fixed red points).

**Goal**: Derive an upper bound m_{t+1} ≤ U(b) for the number of new blue points created on day t→t+1.

**Notation**:
- b = |B_t| (blue points at day t)
- p = 3 + b (total points at day t)
- L_t = set of lines through pairs of points in P_t
- m_{t+1} = |N_t| = number of novel points created on day t+1

---

## DERIVATION

### Step 1: Naive Upper Bound (General Position)

**Assumption**: No collinearities among points in P_t (i.e., no three points on a line).

**Line count**:
$$L = |L_t| = \binom{p}{2} = \binom{3+b}{2} = \frac{(3+b)(2+b)}{2}$$

**Potential intersections**: Every pair of distinct lines intersects at exactly one point (in ℝℙ²).

$$\text{Total intersections} = \binom{L}{2} = \frac{L(L-1)}{2}$$

Expanding with L = (3+b)(2+b)/2:

$$\binom{L}{2} = \frac{(3+b)(2+b)}{2} \cdot \frac{(3+b)(2+b) - 2}{4} = \frac{(3+b)(2+b)[(3+b)(2+b) - 2]}{8}$$

**Justification**: This counts all pairwise line intersections, assuming no three lines concur beyond the existing points.

---

### Step 2: Reduction from Red Pencils

A **pencil** at point r is the set of all lines through r.

**Red point r_i** (for i = 1, 2, 3):
- Lines to the other 2 red points: 2
- Lines to each of the b blue points: b
- **Total degree**: d_{r_i} = 2 + b

**Forced concurrency at r_i**: All lines through r_i intersect pairwise at r_i.

Number of line pairs through r_i:
$$\binom{d_{r_i}}{2} = \binom{2+b}{2} = \frac{(2+b)(1+b)}{2}$$

These intersections are at r_i, which is **already in P_t**, so they do NOT contribute to N_t.

**Total reduction from 3 red pencils**:
$$\Delta_{\text{red}} = 3 \cdot \binom{2+b}{2} = \frac{3(2+b)(1+b)}{2}$$

**Theorem invoked**: **Incidence axiom** (all lines through a point concur at that point).

---

### Step 3: Reduction from Blue Pencils

**Blue point b_j** (for j = 1, ..., b):
- Lines to the 3 red points: 3
- Lines to each of the other (b-1) blue points: b-1
- **Total degree**: d_{b_j} = 3 + (b-1) = 2 + b

**Forced concurrency at b_j**: All lines through b_j intersect pairwise at b_j.

Number of line pairs through b_j:
$$\binom{d_{b_j}}{2} = \binom{2+b}{2} = \frac{(2+b)(1+b)}{2}$$

These are already in P_t, so they do NOT contribute to N_t.

**Total reduction from b blue pencils**:
$$\Delta_{\text{blue}} = b \cdot \binom{2+b}{2} = \frac{b(2+b)(1+b)}{2}$$

**Theorem invoked**: **Incidence axiom** (pencil concurrency).

---

### Step 4: Combined Reduction from All Existing Points

Total reduction from all p = 3 + b existing points:

$$\Delta_{\text{existing}} = \Delta_{\text{red}} + \Delta_{\text{blue}} = (3 + b) \cdot \binom{2+b}{2} = \frac{(3+b)(2+b)(1+b)}{2}$$

**Check**: In general position, each point has degree p-1 = 2+b, so:
$$p \cdot \binom{p-1}{2} = (3+b) \cdot \binom{2+b}{2}$$ ✓

---

### Step 5: Naive Upper Bound Formula

Subtracting forced concurrencies at existing points:

$$m_{t+1} \leq \binom{L}{2} - \Delta_{\text{existing}}$$

$$m_{t+1} \leq \frac{(3+b)(2+b)[(3+b)(2+b) - 2]}{8} - \frac{(3+b)(2+b)(1+b)}{2}$$

Factor out (3+b)(2+b):

$$m_{t+1} \leq (3+b)(2+b) \left[ \frac{(3+b)(2+b) - 2}{8} - \frac{1+b}{2} \right]$$

$$= (3+b)(2+b) \left[ \frac{(3+b)(2+b) - 2 - 4(1+b)}{8} \right]$$

Expand (3+b)(2+b) = 6 + 5b + b²:

$$(3+b)(2+b) - 2 - 4(1+b) = 6 + 5b + b² - 2 - 4 - 4b = b² + b = b(b+1)$$

Therefore:

$$\boxed{m_{t+1} \leq U(b) = \frac{(3+b)(2+b) \cdot b(b+1)}{8}}$$

**Alternative forms**:

1. In terms of p = 3 + b:
   $$U(b) = \frac{p(p-1) \cdot b(b+1)}{8}$$

2. In terms of L (line count):
   $$U(b) = \frac{L \cdot b(b+1)}{4} \quad \text{where } L = \binom{p}{2}$$

---

## Step 6: Additional Reductions (Theorem-Based)

The bound U(b) assumes **general position**: no unexpected collinearities or concurrencies beyond those forced by existing points.

**Additional reductions occur when**:

### (A) Collinearities Reduce Line Count

If k ≥ 3 points become collinear, the line count drops by:
$$\Delta L = \binom{k}{2} - 1$$

This reduces the naive bound since fewer lines mean fewer intersection pairs.

**Theorems that force collinearity**:

1. **Pappus's Hexagon Theorem**:
   - **Setup**: Two distinct lines ℓ, m, each containing ≥3 points
   - **Conclusion**: Certain 3 intersection points are collinear
   - **Reduction**: Δ ≥ C(3,2) - 1 = 2 lines

2. **Pascal's Theorem**:
   - **Setup**: 6 points lie on a conic
   - **Conclusion**: 3 specific intersection points are collinear
   - **Reduction**: Δ ≥ 2 lines

3. **Desargues's Theorem** (collinearity part):
   - **Setup**: Two triangles perspective from a point
   - **Conclusion**: 3 specific intersection points are collinear
   - **Reduction**: Δ ≥ 2 lines

### (B) Novel Concurrencies Reduce Intersection Count

If k ≥ 3 lines concur at a **new** point p ∉ P_t, the intersection count drops by:
$$\Delta N = \binom{k}{2} - 1$$

because k lines in general position would create C(k,2) points, but concurrency creates only 1.

**Theorems that force novel concurrency**:

1. **Desargues's Theorem** (concurrency part):
   - **Setup**: Two triangles perspective from a line
   - **Conclusion**: 3 specific lines are concurrent
   - **Reduction**: Δ ≥ C(3,2) - 1 = 2 points

2. **Brianchon's Theorem**:
   - **Setup**: 6 lines tangent to a conic
   - **Conclusion**: 3 specific lines are concurrent
   - **Reduction**: Δ ≥ 2 points

3. **Ceva's Theorem** (projective version):
   - **Setup**: Three cevians of a triangle satisfy cross-ratio condition
   - **Conclusion**: The three cevians are concurrent
   - **Reduction**: Δ ≥ 2 points

---

## FINAL FORMULA WITH REDUCTIONS

$$\boxed{m_{t+1} \leq U(b) - \Delta_{\text{Pappus}} - \Delta_{\text{Pascal}} - \Delta_{\text{Desargues}} - \Delta_{\text{Brianchon}} - \Delta_{\text{other}}}$$

where:

$$U(b) = \frac{(3+b)(2+b) \cdot b(b+1)}{8}$$

is the **naive upper bound in general position**, and each Δ term is the reduction due to a specific classical theorem.

---

## SUMMARY TABLE

| Component | Formula | Justification |
|-----------|---------|---------------|
| **Total line pairs** | C(L, 2) where L = C(3+b, 2) | General position |
| **Red pencil reduction** | 3 · C(2+b, 2) | Incidence (all lines through r_i concur at r_i) |
| **Blue pencil reduction** | b · C(2+b, 2) | Incidence (all lines through b_j concur at b_j) |
| **Pappus reduction** | ≥ 2 per configuration | Pappus's Hexagon Theorem |
| **Desargues reduction** | ≥ 2 + 2 per configuration | Desargues's Theorem (both parts) |
| **Pascal reduction** | ≥ 2 per configuration | Pascal's Theorem (6 points on conic) |
| **Brianchon reduction** | ≥ 2 per configuration | Brianchon's Theorem (dual of Pascal) |

---

## THEOREMS INVOKED

1. **Incidence Axiom** (projective geometry): All lines through a point concur at that point.
   - Used for: Red and blue pencil reductions

2. **Pappus's Hexagon Theorem**: If A, B, C lie on line ℓ and A', B', C' lie on line m, then the three points AB'∩A'B, AC'∩A'C, BC'∩B'C are collinear.
   - Used for: Collinearity reductions when two lines have ≥3 points each

3. **Desargues's Theorem**: Triangles perspective from a point are perspective from a line, and vice versa.
   - Used for: Both collinearity and concurrency reductions

4. **Pascal's Theorem**: For 6 points on a conic, opposite sides of the hexagon intersect in 3 collinear points.
   - Used for: Collinearity reductions (if points lie on conics)

5. **Brianchon's Theorem**: For 6 lines tangent to a conic, lines joining opposite vertices are concurrent.
   - Used for: Concurrency reductions (dual of Pascal)

---

## NUMERICAL EXAMPLE

**Day 0**: b = 2 (two blue points)

$$U(2) = \frac{(3+2)(2+2) \cdot 2(2+1)}{8} = \frac{5 \cdot 4 \cdot 2 \cdot 3}{8} = \frac{120}{8} = 15$$

So the naive upper bound predicts m_1 ≤ 15 new points on day 1 (before accounting for Pappus, Desargues, etc.).

**Check via direct counting**:
- p_0 = 5 points
- L_0 = C(5,2) = 10 lines (if no collinearities)
- Total intersections: C(10,2) = 45
- Existing points (each degree 4): 5 · C(4,2) = 5 · 6 = 30
- Naive new points: 45 - 30 = 15 ✓

---

## CONCLUSION

The upper bound is:

$$\boxed{m_{t+1} \leq U(b) = \frac{(3+b)(2+b) \cdot b(b+1)}{8}}$$

where b = |B_t|, derived purely from incidence counting with reductions justified by:
- **Incidence axiom** (pencil concurrencies at existing points)
- **Pappus, Desargues, Pascal, Brianchon** (additional collinearities/concurrencies)

The actual value m_{t+1} is typically **much smaller** than U(b) due to the many geometric coincidences that occur in this construction.
