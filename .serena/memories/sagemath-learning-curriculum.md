# SageMath Learning Curriculum for Ketema

**Focus Areas:** Analytical Geometry, Combinatorics, Multi-dimensional Spaces

**Pedagogy:** Interactive, concise instructions, student writes code, instructor guides/critiques only

## Level 1: Foundation

### 1.1 Basics ✓
- Print, variables, arithmetic
- Operators: `+`, `-`, `*`, `/`, `^` (exponentiation)
- Lists, indexing, `sum()`

### 1.2 For Loops
- Iterate over lists
- Range function
- List comprehensions

### 1.3 Functions
- Define functions with `def`
- Return values
- Multiple parameters

### 1.4 Conditionals
- `if`, `elif`, `else`
- Boolean operators
- Comparison operators

## Level 2: Combinatorics (Primary Focus)

### 2.1 Combinations
- `Combinations(n, k)` class
- Generate k-subsets
- Count vs enumerate

### 2.2 Permutations
- `Permutations()` class
- Ordered vs unordered
- Factorial counting

### 2.3 Subsets & Power Sets
- `Subsets()` class
- All possible subsets
- Filtering subsets

### 2.4 Cartesian Products
- `CartesianProduct()` class
- Tuples and pairs
- Multi-set products

### 2.5 Graph Theory Basics
- `Graph()` class
- Vertices and edges
- Combinatorial graph problems

## Level 3: Analytical Geometry

### 3.1 Points & Vectors
- `vector()` constructor
- Point arithmetic
- Distance calculations

### 3.2 Lines
- Line through two points
- Parametric equations
- Intersection problems

### 3.3 Planes
- Plane definitions
- Normal vectors
- Point-plane distance

### 3.4 Multi-dimensional Spaces
- n-dimensional vectors
- Vector spaces
- Linear transformations

### 3.5 Coordinate Systems
- Cartesian, polar, spherical
- Coordinate conversions
- Projections

## Level 4: Advanced Topics (Optional)

### 4.1 Matrix Operations
- Matrix construction
- Determinants, eigenvalues
- Systems of equations

### 4.2 Polytopes
- Convex hulls
- Vertices and facets
- Volume calculations

### 4.3 Symbolic Mathematics
- Symbolic variables
- Algebraic manipulation
- Calculus operations

## Teaching Notes

**Session Format:**
- Student in vim (right pane), instructor in Claude (left pane)
- Student writes all code
- Instructor: concise commands, brief explanations, Socratic questioning
- Use `practice.sage` for exercises
- Run with `:term` → `sage practice.sage`

**Learning Principles:**
- Productive struggle (let student attempt first)
- Immediate feedback on errors
- Build on previous lessons
- Real-world Project Euler context available

**Help Resources:**
- `?function` → Help
- `function[TAB]` → Completions
- `search_doc('keyword')` → Search
- Official docs: https://doc.sagemath.org/

**Current Progress:** Level 1.1 completed (basics)
**Next Lesson:** 1.2 For Loops
