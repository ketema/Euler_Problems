# Project Overview

**Purpose**: Solve Project Euler Problem 957 "Point Genesis" - a geometric problem involving red and blue points

**Tech Stack**:
- Python 3.11+
- Poetry for dependency management
- SymPy for exact rational geometry
- NumPy, SciPy, Shapely for numerical/geometric computations
- NetworkX for graph analysis
- Matplotlib for visualization
- Various symbolic algebra tools (z3-solver, casadi, pymbolic)

**Problem**: Given 3 fixed red points and 2 initial blue points, each day draw lines from reds to blues and mark intersections as new blues. Find g(16) = maximal blue points after 16 days.

**Status**: Multiple approaches attempted, all answers rejected by Project Euler:
- 1778 (quadratic)
- 1,973,818 (quartic)  
- 15,730,302,251,147,551,048 (from git history)

**Key Files**:
- `correct_solver.py`: Main simulation using SymPy exact geometry
- `find_recurrence.py`: Found bilinear recurrence g(n+1) = a·g(n) + b·g(n)·g(n-1) + c
- `FINAL_SOLUTION.md`: Documents failed attempts

**Commands**:
- Run scripts: `poetry run python <script>.py`
- Test: No formal test suite (exploratory problem solving)
