# Symbolic Analysis: Point Genesis

## Language Breakdown

**"Point Genesis"**
- Genesis (Greek γένεσις): origin, creation, **generation**, coming into being
- In mathematics: generating set, generator, generative function

## Mathematical Interpretations

###  1. Field-Theoretic (Von Staudt)
The construction generates points over algebraic closures:
- Start with ℚ-rational points
- Each step adds algebraic extensions
- Points lie in ℚ(√3) (from hyperbola x(x-1) = 3y(y-1))
- Generation = field tower: ℚ ⊂ ℚ(√3) ⊂ ...

### 2. Projective Closure
Given 5 points on conic + construction rules → projective closure
- Like "Compass and Straightedge" constructions
- What is CONSTRUCTIBLE from initial configuration?
- g(n) = # constructible points after n steps

### 3. Matroid/Lattice
Configuration forms geometric lattice
- Points = elements
- Lines = flats
- Closure operator: new points from intersections
- g(n) = rank/size of n-step closure

### 4. Graph-Theoretic
- Vertices = points (red/blue)
- Edges = lines
- g(n) = # vertices after n iterations of "line-intersection" operation
- Bipartite structure: reds (fixed) vs blues (growing)

## The Hyperbola Connection

All 5 initial points lie on: **x(x-1) = 3y(y-1)**

Canonical form: **3(y - 1/2)² - (x - 1/2)² = 1**

**Pascal's Theorem**: 6 points on conic → opposite sides meet collinearly
**Pappus's Theorem**: Degenerate case (2 lines)
**Braikenridge-Maclaurin**: 5 points determine conic uniquely

## Key Question

What if "maximal" means:
- **Option A**: Evolve ONE optimal initial configuration
- **Option B**: At each day n, choose THE optimal configuration that maximizes g(n) **independently**

If Option B: different configs might be optimal for different n!
- g(1)=8: universal for general position
- g(2)=28: specific config
- g(3)=184: possibly different config?
- ...
- g(16)=?: yet another config?

## Theorem Search

Need to find theorem of form:

**Theorem (Hypothetical)**: Given 3 non-collinear points R and 2 points B on hyperbola H, the n-step closure under "red-blue line intersection" contains exactly g(n) = f(n) blue points, where f is...

Candidates:
- Combinatorial formula (choose functions)
- Recurrence with closed form
- Connection to known sequence (Catalan, Fibonacci extended, etc.)
- Modular arithmetic pattern

## Action Items

1. Search for theorems about "iterative constructions on conics"
2. Look for "closure under line-point incidence"
3. Investigate if problem is about ALGEBRAIC DEGREE (ℚ(√3, √...) extensions)
4. Check if answer is asking for degree of field extension, not point count
