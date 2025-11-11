# Complete Toolkit for Problem 957 (For Local Laptop)

## Required Python Libraries

Install these via pip:

```bash
pip install numpy sympy shapely scipy networkx matplotlib oeis
```

### Core Libraries

1. **NumPy** (numerical computation)
   - Exact rational arithmetic
   - Matrix operations
   - Linear algebra

2. **SymPy** (symbolic mathematics) ⭐ KEY
   - Solve recurrence relations: `sympy.rsolve()`
   - Symbolic equation solving
   - Pattern matching
   - Rational number arithmetic
   - Generate closed-form formulas

3. **Shapely** (computational geometry) ⭐ KEY
   - **Deterministic line-line intersection**
   - Point-in-polygon tests
   - Exact geometric predicates
   - Handles degenerate cases correctly
   - No floating-point errors in topology

4. **SciPy** (scientific computing)
   - `scipy.optimize.curve_fit` for symbolic regression
   - Interpolation and extrapolation
   - Statistical tools

5. **NetworkX** (graph theory)
   - Bipartite graphs for point/line incidence
   - State tracking
   - Connectivity algorithms

6. **OEIS** (sequence database)
   - Look up integer sequences
   - Verify patterns against known sequences
   ```python
   from oeis import oeis
   results = oeis.search([2, 8, 28, ...])
   ```

7. **Matplotlib** (visualization - optional)
   - Plot configurations
   - Debug geometry visually

## Additional Tools (Optional but Helpful)

### SageMath (if you want nuclear option)
```bash
# SageMath includes everything above plus:
# - Advanced combinatorics
# - Symbolic geometry
# - Number theory
# - Automated theorem proving
```

### Symbolic Regression Libraries
```bash
pip install gplearn  # Genetic programming for symbolic regression
pip install pysr     # PySR - physics-inspired symbolic regression
```

### Graph Visualization
```bash
pip install graphviz  # For NetworkX visualization
```

## Critical Code Patterns

### 1. Rigorous Geometric Intersection (Shapely)

```python
from shapely.geometry import Point, LineString
from shapely import equals

# Define points
r1 = Point(0, 0)
b1 = Point(1, 1)

# Define lines (infinite lines through two points)
# NOTE: Shapely LineString is a SEGMENT
# For infinite lines, use parametric representation

line1 = LineString([r1, b1])
line2 = LineString([r2, b2])

# Find intersection (deterministic, exact)
intersection = line1.intersection(line2)

if intersection.geom_type == 'Point':
    pt = intersection
    # Check if white
    is_white = not any(equals(pt, existing) for existing in all_points)
```

### 2. Solve Recurrence with SymPy

```python
from sympy import symbols, Function, rsolve, Eq

n = symbols('n')
g = Function('g')

# Example: g(n) = 2*g(n-1) + g(n-2)
recurrence = g(n) - 2*g(n-1) - g(n-2)

# Solve with initial conditions
solution = rsolve(recurrence, g(n), {g(0): 2, g(1): 8})
print(solution)  # Closed-form formula

# Evaluate at n=16
result = solution.subs(n, 16)
```

### 3. Pattern Discovery with OEIS

```python
from oeis import oeis

# Your sequence
sequence = [2, 8, 28, ...]  # g(0), g(1), g(2), ...

# Search OEIS
results = oeis.search(sequence)

for entry in results:
    print(f"A{entry.id}: {entry.name}")
    print(f"  Formula: {entry.formulas}")
    print(f"  Comments: {entry.comments}")
```

### 4. Symbolic Regression (if no pattern found)

```python
from scipy.optimize import curve_fit
import numpy as np

# Data
t_data = np.array([0, 1, 2])
g_data = np.array([2, 8, 28])

# Try different functional forms
def quadratic(t, a, b, c):
    return a*t**2 + b*t + c

def exponential(t, a, b, c):
    return a * b**t + c

# Fit
params, _ = curve_fit(quadratic, t_data, g_data)
print(f"Quadratic fit: g(t) = {params[0]:.0f}t² + {params[1]:.0f}t + {params[2]:.0f}")
```

### 5. State Machine with Explicit Tracking

```python
class GeometricState:
    def __init__(self, reds, blues):
        self.reds = [Point(x, y) for x, y in reds]
        self.blues = [Point(x, y) for x, y in blues]
        self.day = 0

    def step(self):
        # 1. Construct ALL lines
        lines = [(LineString([r, b]), r, b) for r in self.reds for b in self.blues]

        # 2. Find intersections
        new_blues = set()
        for i, (line1, r1, b1) in enumerate(lines):
            for line2, r2, b2 in lines[i+1:]:
                pt = line1.intersection(line2)

                if pt.geom_type == 'Point':
                    # Check if white
                    if self.is_white(pt):
                        new_blues.add((pt.x, pt.y))

        # 3. Update state
        self.blues.extend([Point(x, y) for x, y in new_blues])
        self.day += 1

        return len(new_blues)

    def is_white(self, pt):
        return (not any(equals(pt, r) for r in self.reds) and
                not any(equals(pt, b) for b in self.blues))
```

## Debugging Strategy

1. **Start with rigorous geometry**
   - Use Shapely for ALL intersections
   - Never use manual algebra
   - Trust deterministic predicates

2. **Explicit state tracking**
   - Track which points are red/blue/white
   - Verify constraints at each step
   - Log all intersections with labels

3. **Systematic search**
   - Try standard configurations first:
     - Points on circle
     - Points on conic (parabola, hyperbola)
     - Regular polygons
     - Rational coordinate grids
   - Use symmetry to reduce search space

4. **Pattern discovery**
   - Compute g(0) through g(5) rigorously
   - Look for patterns in differences
   - Try SymPy recurrence solver
   - Search OEIS
   - If no match, problem may have special structure

5. **Verification**
   - Cross-check with multiple methods
   - If simulation and formula disagree, trust simulation
   - If two simulations disagree, debug both

## Key Insights from Cloud Work

- **Manual intersection math had bugs** - always use geometry library
- **Configuration matters** - not all 3-red-2-blue configs give g(1)=8, g(2)=28
- **Sequence may not be simple polynomial** - be prepared for complex patterns
- **The "right" configuration likely has special structure** - conic, symmetry, etc.

## Next Steps

1. Clone the repository with all code
2. Install toolkit on laptop
3. Run `rigorous_simulator.py` to verify setup
4. Implement systematic search with proper geometry
5. Use SymPy to find recurrence once you have enough terms
6. Verify with OEIS

## Files to Focus On

- `rigorous_simulator.py` - Correct geometric implementation
- `TOOLKIT.md` - This capabilities summary
- `debug_rigorous.py` - Shows detailed intersection analysis
- All other `.py` files - attempts with various approaches

Good luck! The key is to **build from first principles, not pattern-match**.
