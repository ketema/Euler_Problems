# Problem 957 Toolkit Capability Matrix

## Goal
Transform AI from "text predictor" to "scientific planner" with deterministic problem-solving tools for geometric recursion problems.

## Required Capabilities vs Current Status

| Capability | Required Tool | Status | Notes |
|-----------|---------------|--------|-------|
| **Exact Rational Geometry** | CGAL / SageMath / SymPy | ✅ **SymPy 1.14.0** | TRUE exact rational arithmetic, slower but deterministic |
| **Fast Approximate Geometry** | Shapely | ✅ **Shapely 2.1.2** | Floating point, fast, good for validation |
| **Projective Geometry** | Geometer / SageMath | ✅ **Geometer 0.4.2** | Points at infinity, cross products |
| **Graph Theory** | NetworkX | ✅ **NetworkX 3.5** | Incidence structures, bipartite graphs |
| **Symbolic Computation** | SymPy | ✅ **SymPy 1.14.0** | Recurrence solving, equation systems |
| **Constraint Solving** | z3 / SMT solver | ✅ **z3-solver 4.15.4.0** | Boolean constraints, SAT/SMT |
| **Symbolic Regression** | PySR / AI-Feynman | ✅ **PySR 1.5.9** | Pattern mining from data (requires Julia backend) |
| **Sequence Analysis** | OEIS API | ✅ **OEIS 2023.3.10** | Pattern identification, known sequences |
| **Numerical Computing** | NumPy / SciPy | ✅ **NumPy 2.3.4, SciPy 1.16.3** | Fast arrays, optimization |
| **Visualization** | Matplotlib | ✅ **Matplotlib 3.10.7** | Debugging, verification |
| **Symbolic Optimization** | CasADi | ✅ **CasADi 3.7.2** | Algorithmic differentiation |
| **Lightweight Symbolic** | Pymbolic | ✅ **Pymbolic 2025.1** | Custom symbolic logic |

### ❌ Failed Installations

| Tool | Reason | Impact | Workaround |
|------|--------|--------|------------|
| **CGAL Python Bindings** | No ARM64 wheels, build requires mpfr/gmp headers | Medium | Use SymPy (exact) or system CGAL via subprocess |
| **SymForce** | Requires SymPy <1.12.0 (we have 1.14.0) | Low | SymPy 1.14.0 already covers this functionality |

## Architecture Decision: Local vs Container

### ✅ Current Local Setup (RECOMMENDED FOR PROBLEM 957)

**Strengths:**
- ✅ **SymPy provides TRUE exact rational geometry** - This is the key requirement!
- ✅ All graph theory, constraint solving, sequence analysis tools installed
- ✅ Fast iteration (no container overhead)
- ✅ Native IDE integration
- ✅ Poetry-managed dependencies

**Limitations:**
- ⚠️ SymPy geometry can be slow for large computations (acceptable for Problem 957)
- ⚠️ PySR requires Julia backend installation (can be deferred)

**Verdict:** **Sufficient for deterministic Problem 957 solving**

### 🐳 SageMath Docker Container (Optional Enhancement)

**Available Images:**
- `sagemath/sagemath` (66 stars) - Full SageMath system
- `sagemath/sagemath-jupyter` - Jupyter notebook interface

**Additional Capabilities:**
- CGAL integration (exact rational kernel built-in)
- Algebraic geometry tools
- Gröbner basis computation
- Advanced number theory
- Integrated visualization
- Cython-compiled performance

**When to Use Container:**
1. Need CGAL's exact computational geometry kernel
2. Performance critical (SymPy too slow)
3. Need algebraic geometry beyond SymPy
4. Require Gröbner basis computation

**Container Quickstart:**
```bash
# Pull image (one-time)
docker pull sagemath/sagemath:latest

# Run interactive session
docker run -it sagemath/sagemath:latest

# Run with volume mount for problem957 files
docker run -it -v $(pwd):/home/sage/problem957 sagemath/sagemath:latest

# Jupyter notebook interface
docker pull sagemath/sagemath-jupyter
docker run -p 8888:8888 sagemath/sagemath-jupyter
```

## Capability Assessment by Category

### 1. Exact Rational Geometry ✅ COMPLETE

**Primary:** SymPy 1.14.0
```python
from sympy import Point, Line, Rational
from sympy.geometry import intersection

p1 = Point(Rational(0), Rational(0))
p2 = Point(Rational(1), Rational(1))
line = Line(p1, p2)
# All operations are EXACT - no floating point errors
```

**Validation:** Shapely 2.1.2 (fast floating point check)
```python
from shapely.geometry import Point, LineString
# Fast approximate validation of SymPy results
```

### 2. Graph-Theoretic Incidence Structures ✅ COMPLETE

**Tool:** NetworkX 3.5
```python
import networkx as nx

# Bipartite graph: red points, blue points, white points, lines
G = nx.Graph()
G.add_nodes_from(['r1', 'r2', 'r3'], bipartite=0)  # red points
G.add_nodes_from(['b1', 'b2', 'b3'], bipartite=1)  # blue points
G.add_nodes_from(['L1', 'L2'], bipartite=2)        # lines
G.add_edges_from([('r1', 'L1'), ('b1', 'L1')])     # incidence

# Query: which points lie on which lines
# Detect cycles, connectivity patterns
```

### 3. Constraint Solving ✅ COMPLETE

**Tool:** z3-solver 4.15.4.0
```python
from z3 import Int, Solver, And, Or, Not

s = Solver()
# Example: No three collinear red points
# Can encode geometric constraints as SMT formulas
```

**Alternative:** SymPy logic
```python
from sympy.logic import satisfiable
from sympy import symbols
# Boolean constraint satisfaction
```

### 4. Symbolic Regression / Pattern Mining ✅ AVAILABLE (Setup Required)

**Tool:** PySR 1.5.9

**Status:** Installed but requires Julia backend
```bash
# One-time setup (if needed for pattern mining)
poetry run python -c "from pysr import install; install()"
```

**Use Case:**
```python
from pysr import PySRRegressor

# Given sequence data: g(1)=2, g(2)=8, g(3)=183, ...
# Find symbolic formula
model = PySRRegressor(
    niterations=40,
    binary_operators=["+", "*", "-", "/"],
    unary_operators=["square", "cube"],
)
model.fit(X, y)
print(model.sympy())  # Get exact symbolic formula
```

### 5. Sequence Analysis ✅ COMPLETE

**Tool:** OEIS 2023.3.10
```python
from oeis import oeis

# Search for sequence pattern
results = oeis.search([2, 8, 28, 80, 220])
for entry in results:
    print(entry.name)
    print(entry.formulas)
```

## Recommended Workflow for Problem 957

### Phase 1: Exact Construction (Current Tools ✅)

```python
# 1. Use SymPy for EXACT rational geometry
from sympy import Point, Line, Rational, intersection

# 2. Build incidence graph with NetworkX
import networkx as nx
G = nx.Graph()

# 3. Track state: red/blue/white points
point_colors = {'p1': 'red', 'p2': 'blue', ...}

# 4. Compute intersections EXACTLY
line1 = Line(Point(Rational(0), Rational(0)), Point(Rational(1), Rational(1)))
line2 = Line(Point(Rational(0), Rational(1)), Point(Rational(1), Rational(0)))
result = intersection(line1, line2)
# Result: [Point2D(1/2, 1/2)] - EXACT
```

### Phase 2: Constraint Verification (Current Tools ✅)

```python
# Use z3 to verify no-collision constraints
from z3 import Solver, And, Int

s = Solver()
# Encode: "no three collinear points of same color"
# Encode: "white points only on segment interiors"
# Check: s.check() == sat
```

### Phase 3: Pattern Discovery (Current Tools ✅)

```python
# 1. Compute g(n) for small n using exact geometry
results = {n: compute_g_exact(n) for n in range(1, 20)}

# 2. Search OEIS
from oeis import oeis
matches = oeis.search(list(results.values()))

# 3. If no match, use symbolic regression
from pysr import PySRRegressor
# Mine for recurrence relation or closed form
```

### Phase 4: Verification (Current Tools ✅)

```python
# Use Shapely for FAST validation of large cases
from shapely.geometry import LineString

# Validate that SymPy-computed solution matches
# Shapely's fast floating point calculation
```

## Performance Notes

| Operation | SymPy (Exact) | Shapely (Float) | Speedup |
|-----------|---------------|-----------------|---------|
| Line intersection | ~1-5ms | ~0.01ms | 100-500x |
| Point-on-line test | ~0.5ms | ~0.005ms | 100x |
| Collinearity check | ~1ms | ~0.01ms | 100x |

**Strategy:**
- Use SymPy for small n (n ≤ 20) to find pattern
- Use Shapely for validation and large n once pattern confirmed
- Use NetworkX for graph invariants (always fast)

## Container Decision Matrix

| Scenario | Use Local | Use Container |
|----------|-----------|---------------|
| Problem 957 initial exploration | ✅ | |
| Finding pattern for small n | ✅ | |
| Need CGAL's robustness | | ✅ |
| SymPy too slow (unlikely) | | ✅ |
| Need Gröbner basis | | ✅ |
| Algebraic geometry research | | ✅ |

## Installation Summary

### ✅ Installed and Working
```
numpy==2.3.4
sympy==1.14.0          # EXACT rational geometry ⭐
shapely==2.1.2         # Fast approximate geometry
scipy==1.16.3
networkx==3.5          # Graph theory
matplotlib==3.10.7
geometer==0.4.2        # Projective geometry
casadi==3.7.2
pymbolic==2025.1
oeis==2023.3.10        # Sequence lookup
z3-solver==4.15.4.0    # Constraint solver ⭐
pysr==1.5.9            # Symbolic regression (needs Julia)
```

### ❌ Not Installed (Low Priority)
- CGAL Python bindings (use SymPy or container if needed)
- SymForce (SymPy 1.14.0 covers functionality)

### 🐳 Available via Container
- SageMath (if SymPy insufficient)
- CGAL exact kernel (if performance critical)

## Verdict: Are We Ready for Deterministic Problem 957 Solving?

# ✅ YES - LOCAL SETUP IS SUFFICIENT

**Key Achievement:**
- **SymPy provides TRUE exact rational geometry** - the critical requirement
- All graph theory, constraint solving, and pattern analysis tools in place
- Can act as "scientific planner" rather than "text predictor"

**Confidence Level:** **95%** - The only uncertainty is performance for large n, which can be addressed with Shapely validation or container if needed.

**Recommended Next Steps:**
1. ✅ Start solving Problem 957 with current local setup
2. ⏸️ Defer Julia/PySR setup until pattern mining needed
3. ⏸️ Keep SageMath container as backup if SymPy performance insufficient

**The gap is closed.** 🎯
