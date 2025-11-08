# Implementation Plan: Project Euler Problem 957 POC

**Date**: 2025-11-06
**Branch**: feature/phase2-enforcement
**Objective**: Validate TDD Subagent Architecture using Project Euler Problem 957
**Requirements**: prototypes/problem957/requirements/requirements.md

---

## Executive Summary

This POC validates the TDD Subagent Architecture by implementing Project Euler Problem 957 (Point Genesis). The problem involves computational geometry (line intersections, point propagation) with known verification values (g(1)=8, g(2)=28) and requires calculating g(16).

**Key Validation Goals:**
1. Test adversarial test-writer/coder separation
2. Measure coordinator context savings (target: 70%+)
3. Verify self-documenting error message quality gate
4. Validate refactor iteration cycle (if needed)
5. Measure total system token usage

---

## Architecture Overview

### Phase 1: Geometry Primitives (TDD Cycle 1)

**Test-Writer Subagent** (blind to implementation):
- Write tests for Point class (REQ-PE957-001)
- Write tests for Line class (REQ-PE957-002)
- Write tests for line intersection (REQ-PE957-003)
- Error messages must pass 5-point quality standard
- Token budget: 15-20K

**Coder Subagent** (blind to test source):
- Implement Point class from error messages
- Implement Line class from error messages
- Implement intersection algorithm from error messages
- Token budget: 20-25K

**Files Created:**
- `src/geometry.py` (Point, Line, intersect)
- `tests/test_geometry.py` (unit tests)

**Success Criteria:**
- All geometry tests GREEN
- Point equality with epsilon tolerance works
- Line intersection handles vertical lines, parallel lines
- No imports beyond standard library

---

### Phase 2: Propagation Logic (TDD Cycle 2)

**Test-Writer Subagent**:
- Write tests for single day propagation (REQ-PE957-004)
- Test edge cases: parallel lines, overlapping points, deduplication
- Error messages include fix guidance
- Token budget: 8-12K

**Coder Subagent**:
- Implement propagate_one_day() function
- Use geometry primitives from Phase 1
- Handle all edge cases per error messages
- Token budget: 10-15K

**Files Created/Modified:**
- `src/propagation.py` (propagate_one_day)
- `tests/test_propagation.py` (unit tests)

**Success Criteria:**
- Propagation tests GREEN
- Generates correct intersection count
- Excludes already-colored points
- Deduplicates overlapping intersections

---

### Phase 3: Multi-Day Simulation (TDD Cycle 3)

**Test-Writer Subagent**:
- Write tests for multi-day simulation (REQ-PE957-005)
- Test g(1)=8 with specific configuration (REQ-PE957-006)
- Test g(2)=28 with same configuration
- Test monotonic increase property
- Token budget: 8-12K

**Coder Subagent**:
- Implement simulate_days(red, blue, n) function
- Iterate propagation n times
- Return final blue count
- Token budget: 8-12K

**Files Created/Modified:**
- `src/simulation.py` (simulate_days, initial_config)
- `tests/test_simulation.py` (integration tests)

**Success Criteria:**
- g(1) = 8 verified
- g(2) = 28 verified
- g(16) calculated successfully
- All simulation tests GREEN

---

### Phase 4: Web Visualization (TDD Cycle 4 - AI BOARD FEEDBACK APPLIED)

**Test-Writer Subagent**:
- Write tests for data export (REQ-PE957-007)
- Test JSON structure validation
- Test coordinate transformation logic
- Error messages pass 5-point standard
- Token budget: 8-10K

**Coder Subagent**:
- Implement data export from simulation
- Create HTML page with Canvas API
- Implement visualize.js (canvas rendering)
- Handle coordinate transformation
- Token budget: 10-12K

**Files Created:**
- `web/index.html` (visualization page)
- `web/visualize.js` (canvas drawing logic)
- `web/data.json` (export final state from Python)
- `tests/test_visualization.py` (NEW - TDD cycle added per AI Board)

**Success Criteria:**
- All visualization tests GREEN
- Opens in browser without web server
- Shows all points colored correctly
- Displays g(16) result
- Token budget: 23-27K (includes refactor 5K)

---

## TDD Workflow Detail

### M3: Plan (Current Phase)
1. ✓ Create requirements document
2. ✓ Create implementation plan (this document)
3. → Submit to AI Board PARALLEL for critique
4. → Apply all AI Board feedback
5. → Get human approval
6. → Create TSR for test-writer (Phase 1)

### M4: Implement (Iterative Cycles)

**For each phase (1-3):**

1. **RED Phase**:
   - Coordinator creates TSR from requirements
   - Coordinator spawns test-writer subagent (fire-and-forget)
   - Test-writer writes failing tests
   - Test-writer validates error messages with AI Board ONESHOT
   - Coordinator spot-checks 3 error messages
   - Verify: pytest shows RED (all tests failing)

2. **GREEN Phase**:
   - Coordinator spawns coder subagent (fire-and-forget)
   - Coder implements from error messages only (no test source access)
   - Coder consults AI Board ONESHOT for design decisions (if needed)
   - Coder returns: implementation + GREEN confirmation
   - Verify: pytest shows GREEN (all tests passing)

3. **COMMIT Phase**:
   - Coordinator commits with WHY/EXPECTED message
   - Record evidence: git hash, test results, coverage

4. **REFACTOR Phase** (if needed):
   - If tests fail after GREEN: coordinator arbitrates
   - Evaluate test_sound and impl_sound
   - Spawn refactor-coder or refactor-test-writer as needed
   - Maximum 3 iterations, else escalate to user

### M5: Validate
1. Run full test suite (all phases)
2. Verify coverage >85%
3. Calculate g(16) and display in web interface
4. Measure metrics: coordinator savings, system savings, iterations
5. Submit final code to AI Board PARALLEL
6. Apply all feedback
7. Commit final state

---

## Design Decisions

### D1: Use Python Standard Library Only
**Rationale**: Minimize dependencies, demonstrate TDD can work without specialized libraries
**Impact**: Implement line intersection algorithm manually (educational value)
**Alternative Considered**: NumPy for linear algebra (rejected: adds complexity)

### D2: Epsilon Tolerance for Float Equality
**Value**: 1e-9
**Rationale**: Balances precision with robustness for this problem scale
**Impact**: Point class needs custom __eq__ and __hash__

### D3: Functional Design Pattern
**Approach**: Pure functions for geometry operations
**Rationale**: QS2 (functional patterns), easier to test, no side effects
**Impact**: Pass point sets explicitly, return new sets (immutable)

### D4: Three Separate Modules
**Files**: geometry.py, propagation.py, simulation.py
**Rationale**: QS2 (separation of concerns), QS4 (file size <500 lines)
**Impact**: Clear dependencies: simulation → propagation → geometry

### D5: Hard-Coded Initial Configuration
**Approach**: Symmetric placement (equilateral triangle + 2 on axis)
**Rationale**: Out-of-scope: optimization (OOS-PE957-001)
**Impact**: May not achieve absolute maximum g(n), but sufficient for validation

---

## Risk Assessment

### R1: g(1)=8 / g(2)=28 Not Achieved
**Likelihood**: Medium
**Impact**: High (blocks validation)
**Mitigation**:
- Use known symmetric configuration
- Verify geometry implementation carefully
- Consult AI Board if discrepancy found

### R2: Refactor Iterations Exceed Limit
**Likelihood**: Low
**Impact**: Medium (requires user intervention)
**Mitigation**:
- Comprehensive TSR reduces test-writer errors
- Error message quality gate reduces coder confusion
- Coordinator arbitration logic handles most cases

### R3: Token Budget Exceeded
**Likelihood**: Low
**Impact**: Medium (breaks POC constraints)
**Mitigation**:
- Estimated total: 52-63K (within 110K remaining)
- Monitor usage after each subagent spawn
- Can reduce web viz complexity if needed

### R4: Floating Point Precision Errors
**Likelihood**: Medium
**Impact**: Low (test failures, but fixable)
**Mitigation**:
- Epsilon tolerance in Point equality
- Use rational arithmetic if needed (fractions module)
- Verify with exact cases first (integer coordinates)

---

## Token Budget Breakdown

| Phase | Component | Estimated Tokens |
|-------|-----------|------------------|
| M3 | Requirements doc | 2,000 |
| M3 | Implementation plan | 1,500 |
| M3 | AI Board critique | 15,000 |
| M3 | Handoff prep memory | 5,000 |
| M3 | TSR creation (Phase 1) | 1,500 |
| M4 | Test-writer (Phase 1) | 18,000 |
| M4 | Coder (Phase 1) | 22,000 |
| M4 | **Refactor (Phase 1)** | **10,000** |
| M4 | Test-writer (Phase 2) | 10,000 |
| M4 | Coder (Phase 2) | 12,000 |
| M4 | **Refactor (Phase 2)** | **10,000** |
| M4 | **Config pre-validation** | **5,000** |
| M4 | Test-writer (Phase 3) | 10,000 |
| M4 | Coder (Phase 3) | 10,000 |
| M4 | **Refactor (Phase 3)** | **10,000** |
| M4 | **Test-writer (Phase 4)** | **9,000** |
| M4 | **Coder (Phase 4)** | **11,000** |
| M4 | **Refactor (Phase 4)** | **5,000** |
| M4 | Coordinator orchestration | 4,000 |
| M5 | Final AI Board validation | 15,000 |
| M5 | Metrics measurement | 2,000 |
| **Total** | | **178,000** |

**Current Budget**: 81,209 remaining
**Expected Overage**: 96,791 tokens (119%)
**Strategy**: Proactive handoff at 180K (90% threshold) per constitutional protocol

**Mitigation Strategy**:
- If exceeding 90% budget (180K), invoke proactive handoff
- Can simplify web visualization (10K → 5K)
- Can skip final AI Board validation if budget tight (15K savings)
- Refactor iterations not included (contingency: 10-20K if needed)

---

## Success Metrics

### Primary (TDD Subagent Architecture):
1. **Coordinator Context Savings**: >70% vs baseline (45K)
   - Baseline: Coordinator does all work alone
   - Target: <13.5K coordinator context usage
   - Measured: TSR + spot-check + arbitration + orchestration

2. **System Token Savings**: >30% vs baseline
   - Baseline: 45K (coordinator only)
   - Target: <31.5K total system (coordinator + subagents)
   - Measured: All token usage summed

3. **Test Quality**: Error messages pass 5-point standard
   - Measured: AI Board ONESHOT validation + spot-check

4. **Zero Scope Creep**: Test-writer stays within TSR boundaries
   - Measured: Manual review of test file

5. **Refactor Iterations**: ≤1 iteration per phase (ideally 0)
   - Measured: Coordinator arbitration log

### Secondary (Problem 957 Solution):
1. **Correctness**: g(1)=8, g(2)=28, g(16) calculated
2. **Coverage**: >85% line coverage
3. **Quality**: QS2 (DRY/SoC/functional), QS4 (<500 lines/file)
4. **Visualization**: Web interface displays final state

---

## Constraints

### Technical:
- Python 3.10+ standard library only
- No NumPy, no external geometry libraries
- pytest for testing
- HTML5 Canvas for visualization (no frameworks)

### Process:
- Follow M3 → M4 → M5 macros strictly
- Test-writer CANNOT see implementation code
- Coder CANNOT see test source code
- Fire-and-forget subagent constraint (no mid-flight communication)
- Error messages must be self-documenting (5-point standard)

### Quality:
- QS1: >85% test coverage, RED → GREEN → COMMIT → REFACTOR
- QS2: DRY, SoC, functional patterns
- QS4: ≤500 lines per file
- QS5: Ephemeral data only (no persistence in tests)

---

## Dependencies and Assumptions

### Dependencies:
- Python 3.10+ runtime
- pytest installed
- Modern web browser (Chrome, Firefox, Safari)
- Git for version control

### Assumptions:
- Known configuration exists that achieves g(1)=8, g(2)=28
- Epsilon tolerance 1e-9 is sufficient for this problem
- 16 days is computationally tractable (< 1 second runtime)
- Web visualization can run without backend server
- User will manually open HTML file in browser

---

## AI Board Feedback Applied

**Conversation ID**: eb46641f-42f3-44d8-abc0-23a05f5dcc2d
**Date**: 2025-11-06
**Providers**: OpenAI GPT-4.1, Anthropic Claude Sonnet 4.5, Google Gemini 2.5 Flash
**Score**: 6/10 (unanimous - Medium complexity, Medium feasibility)

**Applied Recommendations**:
1. ✓ **Phase 4 TDD Cycle Added**: Web visualization now uses test-writer → coder cycle (was coordinator-led)
2. ✓ **Explicit Refactor Budgets**: 10K per phase (1-3), 5K for phase 4 = 35K total allocated
3. ✓ **Config Pre-Validation**: 5K budget for verification step before Phase 3
4. ✓ **Budget Overage Accepted**: User explicitly authorized full scope, no reduction permitted
5. ✓ **Handoff Preparation**: Comprehensive Serena memory created for auto-compaction recovery

**Deferred Recommendations**:
- Phase 1+2 combination (increases subagent complexity, defer)

**User Authorization**: "I do not authorize ANY scope reduction. My task stands as originally stated. Accomplish it to the best of your ability utilizing constitutional law and test driven development."

## Open Questions

None. User approved full scope with explicit acceptance of auto-compaction risk.

---

## Next Steps

1. ✓ Requirements document created
2. ✓ Implementation plan created (this document)
3. → Submit both to AI Board PARALLEL mode
4. → Apply all AI Board feedback
5. → Get user approval
6. → Create TSR for Phase 1 (geometry primitives)
7. → Spawn test-writer subagent (Phase 1 RED)
8. → Continue through M4/M5 macros

---

## References

- Project Euler Problem 957: https://projecteuler.net/problem=957
- Requirements: prototypes/problem957/requirements/requirements.md
- TDD Subagent Architecture: /.claude/agents/
- TSR Template: /.claude/helpers/tsr-template.md
- Tool Syntax Reference: /.claude/helpers/tool-syntax.md
- Constitutional Law: CLAUDE.md, CL1-CL9, QS1-QS6, M1-M5
