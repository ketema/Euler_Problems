# Project Euler #28 - Number Spiral Diagonals
## Implementation Plan (F#)

**Language**: F# (NEW - not used in Euler_Problems project before)
**Branch**: feature/problem28-fsharp-spiral
**Date**: 2025-01-18

---

## 1. PROBLEM ANALYSIS

### Problem Statement
Calculate the sum of numbers on both diagonals in a 1001×1001 number spiral formed by starting with 1 and moving clockwise.

### Example (5×5 spiral)
```
21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13
```
Diagonal sum: 101

### Mathematical Pattern Analysis

**Spiral Construction**:
- Center: 1 (ring 0)
- Each ring n has side length: 2n + 1
- Each ring n has increment between corners: 2n
- Four corners per ring (except center)

**Ring Pattern**:
- Ring 0 (1×1): center = 1
- Ring 1 (3×3): corners = 3, 5, 7, 9 (start from 1, add 2 each time)
- Ring 2 (5×5): corners = 13, 17, 21, 25 (start from 9, add 4 each time)
- Ring n: increment = 2n, 4 corners

**Target**: 1001×1001 grid
- Side length: 1001 = 2n + 1 → n = 500
- Sum diagonals for rings 0 through 500

### Algorithm
```fsharp
let calculateDiagonalSum gridSize =
    let numRings = (gridSize - 1) / 2
    let rec sumRings ring currentNum acc =
        if ring > numRings then acc
        else
            let increment = 2 * ring
            let corners = [1..4] |> List.scan (fun num _ -> num + increment) currentNum |> List.tail
            let lastCorner = List.last corners
            let ringSum = List.sum corners
            sumRings (ring + 1) lastCorner (acc + ringSum)
    if gridSize = 1 then 1
    else sumRings 1 1 1
```

---

## 2. PROJECT STRUCTURE

### Directory Layout
```
problem28/
├── fsharp/
│   ├── src/
│   │   └── SpiralDiagonal.fs      # Core implementation
│   ├── tests/
│   │   └── SpiralDiagonalTests.fs # Test suite (Expecto)
│   ├── SpiralDiagonal.fsproj      # F# project file
│   ├── paket.dependencies         # Dependencies (or use .NET CLI)
│   └── README.md                  # Documentation
└── IMPLEMENTATION_PLAN.md         # This file
```

### Technology Stack
- **Language**: F# 8.0+ (.NET 8 SDK)
- **Testing**: Expecto (F# native, excellent for functional testing)
- **Build**: .NET CLI (`dotnet`)
- **Coverage**: coverlet (integrated with `dotnet test`)

### Project File (.fsproj)
```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="src/SpiralDiagonal.fs" />
  </ItemGroup>

  <ItemGroup>
    <PackageReference Include="Expecto" Version="10.2.1" />
    <PackageReference Include="coverlet.collector" Version="6.0.0" />
  </ItemGroup>
</Project>
```

---

## 3. DESIGN PATTERNS

### PoEAA Patterns Applied
- **Transaction Script**: Pure functional computation
- **Value Object**: Immutable spiral configuration
- **Service Layer**: Module-based service (F# module system)

### Design Principles (QS2 - Perfectly Aligned with F#)
- **DRY**: Single recursive algorithm
- **Separation of Concerns**:
  - Validation in separate function
  - Core algorithm isolated
  - Test isolation
- **Functional Style** (F# native strengths):
  - **Pure functions**: No side effects
  - **Immutable data**: All values immutable by default
  - **Explicit errors**: Result<'T, 'Error> or exceptions
  - **Type-safe**: Strong static typing with type inference

### YAGNI Compliance
- Console application only
- Core computation with tests
- No unnecessary abstractions

---

## 4. TEST SPECIFICATION (TDD)

### Test Cases (Comprehensive)

**Basic Functionality**:
1. 1×1 grid (n=0) → sum = 1
2. 3×3 grid (n=1) → sum = 25 (1 + 3 + 5 + 7 + 9)
3. 5×5 grid (n=2) → sum = 101 (example from problem)
4. 7×7 grid (n=3) → verify pattern continuation

**Edge Cases**:
1. Invalid input: even number (e.g., 4) → ArgumentException "Grid size must be odd"
2. Invalid input: negative number (e.g., -5) → ArgumentException "Grid size must be positive"
3. Invalid input: zero (0) → ArgumentException "Grid size must be positive"

**Production Case**:
1. 1001×1001 grid → compute actual answer

**Stress Tests** (AI Panel Recommendation #2):
1. 9999×9999 grid → verify no integer overflow, performance <500ms

**Performance**:
1. Large grid computation completes in <100ms (1001×1001)
2. Very large grid computation completes in <500ms (9999×9999)

### Expected Test Coverage
- Function coverage: 100%
- Branch coverage: >90%
- Line coverage: >85%

### Test Structure (Expecto)
```fsharp
module SpiralDiagonalTests

open Expecto
open SpiralDiagonal

[<Tests>]
let tests =
    testList "SpiralDiagonal.calculateDiagonalSum" [
        testList "valid odd grid sizes" [
            test "1x1 grid returns 1" {
                let result = calculateDiagonalSum 1
                Expect.equal result 1 "Center should be 1"
            }

            test "3x3 grid returns 25" {
                let result = calculateDiagonalSum 3
                Expect.equal result 25 "3x3 diagonal sum should be 25"
            }

            test "5x5 grid returns 101" {
                let result = calculateDiagonalSum 5
                Expect.equal result 101 "5x5 diagonal sum should be 101"
            }
        ]

        testList "invalid inputs" [
            test "even number throws exception" {
                Expect.throws (fun () -> calculateDiagonalSum 4 |> ignore)
                    "Even grid size should throw"
            }

            test "negative number throws exception" {
                Expect.throws (fun () -> calculateDiagonalSum -5 |> ignore)
                    "Negative grid size should throw"
            }

            test "zero throws exception" {
                Expect.throws (fun () -> calculateDiagonalSum 0 |> ignore)
                    "Zero grid size should throw"
            }
        ]

        testList "production case" [
            test "1001x1001 grid computes answer" {
                let result = calculateDiagonalSum 1001
                Expect.isGreaterThan result 0 "Answer should be positive"
            }
        ]
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests
```

### Integer Size Analysis (Overflow Prevention)

**AI Panel Recommendation #3**: Document integer size assumptions and overflow handling.

**Data Type**: F# `int` (64-bit signed integer)
**Range**: -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807

**Maximum Values for Target Grids**:

*1001×1001 Grid (Project Euler #28 requirement)*:
- Number of rings: 500
- Last ring increment: 2 × 500 = 1000
- Last corner value: ≈ 1,002,001
- Approximate diagonal sum: ≈ 669,171,001

*9999×9999 Grid (Stress test)*:
- Number of rings: 4999
- Last ring increment: 2 × 4999 = 9998
- Last corner value: ≈ 99,980,001
- Approximate diagonal sum: ≈ 666,566,671,667

**Mathematical Proof of Sufficiency**:
- Maximum value for any grid in testing: ~667 billion (9999×9999 diagonal sum)
- Int64.MaxValue: ~9.2 quintillion (9,223,372,036,854,775,807)
- Margin: >13,000× safety factor

**Conclusion**: 64-bit `int` is **sufficient** for all test cases including stress tests.
- No `BigInteger` needed
- No checked arithmetic required
- No overflow possible within test parameters

**Overflow Detection**: Stress test (9999×9999) validates no overflow occurs in practice.

---

## 5. IMPLEMENTATION APPROACH

### Input Validation Strategy

**AI Panel Recommendation #1**: Explicitly define input validation for all invalid grid sizes.

**Validation Requirements**:
1. **Type Safety**: F# type system ensures `gridSize` parameter is `int` (compile-time enforcement)
2. **Positive Check**: `gridSize > 0` (throws `ArgumentException` with message "Grid size must be positive")
3. **Odd Check**: `gridSize % 2 = 1` (throws `ArgumentException` with message "Grid size must be odd")

**Invalid Inputs and Exceptions**:
- **Zero**: `calculateDiagonalSum 0` → `ArgumentException: "Grid size must be positive"`
- **Negative**: `calculateDiagonalSum -5` → `ArgumentException: "Grid size must be positive"`
- **Even**: `calculateDiagonalSum 4` → `ArgumentException: "Grid size must be odd"`
- **Non-integer**: Prevented by F# type system at compile-time (type error, not runtime exception)

**Validation Location**: `validateGridSize` private function, called before computation begins.

### Core Module Design
```fsharp
module SpiralDiagonal

/// Validates that grid size is a positive odd integer
let private validateGridSize gridSize =
    if gridSize <= 0 then
        invalidArg (nameof gridSize) "Grid size must be positive"
    if gridSize % 2 = 0 then
        invalidArg (nameof gridSize) "Grid size must be odd"

/// Calculate sum of diagonals for n×n spiral grid
/// Throws ArgumentException if gridSize invalid
let calculateDiagonalSum gridSize =
    validateGridSize gridSize

    if gridSize = 1 then 1
    else
        let numRings = (gridSize - 1) / 2

        let rec sumRings ring currentNum acc =
            if ring > numRings then acc
            else
                let increment = 2 * ring
                // Generate 4 corners for this ring
                let mutable num = currentNum
                let mutable ringSum = 0
                for _ in 1..4 do
                    num <- num + increment
                    ringSum <- ringSum + num
                sumRings (ring + 1) num (acc + ringSum)

        sumRings 1 1 1  // Start with ring 1, current number 1, acc 1 (center)

// Entry point for console application
[<EntryPoint>]
let main argv =
    try
        let result = calculateDiagonalSum 1001
        printfn "Project Euler #28 Answer: %d" result
        0 // Exit code
    with
    | ex ->
        eprintfn "Error: %s" ex.Message
        1
```

### Algorithm Walkthrough (5×5 example)
```
Initial: acc = 1 (center), currentNum = 1
Ring 1 (increment = 2):
  num = 1 + 2 = 3, ringSum = 3
  num = 3 + 2 = 5, ringSum = 8
  num = 5 + 2 = 7, ringSum = 15
  num = 7 + 2 = 9, ringSum = 24
  acc = 1 + 24 = 25, currentNum = 9

Ring 2 (increment = 4):
  num = 9 + 4 = 13, ringSum = 13
  num = 13 + 4 = 17, ringSum = 30
  num = 17 + 4 = 21, ringSum = 51
  num = 21 + 4 = 25, ringSum = 76
  acc = 25 + 76 = 101, currentNum = 25

Final: 101 ✓
```

---

## 6. TDD WORKFLOW (M4)

### RED Phase (test-writer sub-agent)
**Task**: Write failing tests WITHOUT seeing implementation
**Input**: This plan + test specification section
**Output**:
- `tests/SpiralDiagonalTests.fs` with Expecto tests
- Self-documenting 5-point error messages:
  1. What failed (test name)
  2. Why (requirement violated)
  3. Expected behavior
  4. Actual behavior (module/function not found)
  5. Guidance (implement SpiralDiagonal module with calculateDiagonalSum function)

### GREEN Phase (coder sub-agent)
**Task**: Implement to pass tests WITHOUT seeing test source
**Input**: Error messages from `dotnet test` only
**Output**:
- `src/SpiralDiagonal.fs` with implementation
- All tests passing
- Git commit with WHY/EXPECTED format

### Iteration Cycle
- If tests fail after GREEN: Use Decision Matrix
- If error messages unclear: Invoke refactor-test-writer
- If implementation wrong: Invoke refactor-coder

---

## 7. QUALITY GATES

### Pre-Implementation (M3)
- ✅ Plan created with F# (NEW language)
- ✅ AI Panel critique received
- ✅ User approval obtained

### Implementation (M4)
- ✅ All tests passing (N/N)
- ✅ Coverage >85% (via coverlet)
- ✅ F# compiler warnings resolved
- ✅ Git commits with WHY/EXPECTED format

### Post-Implementation (M5)
- ✅ AI Panel code review
- ✅ Constitutional compliance audit
- ✅ Evidence documented
- ✅ Memory updated
- ✅ Orchestrator notified

### Constitutional Compliance Traceability Matrix

**AI Panel Recommendation #4**: Map constitutional requirements to implementation steps for auditability.

| Constitutional Requirement | Implementation Location | Evidence Format |
|---------------------------|------------------------|-----------------|
| **CL1**: Instruction Primacy | All sections follow guidelines, no deviations | Full plan adherence |
| **CL2**: Completion Gates | Section 7 (Quality Gates), Section 8 (Completion Criteria) | Checkboxes, test results |
| **CL3**: No Shortcuts | Section 5 (full implementation), Section 9 (no stub mitigations) | Code completeness |
| **CL4**: Self-Monitoring | Section 7.5 (this matrix), checkpoints throughout | Think tool calls |
| **CL5**: Human Approval | Section 7 M3 gate (orchestrator approval) | Approval in task-euler-28-m3-plan.md |
| **CL6**: TDD Enforcement | Section 6 (TDD Workflow), sub-agents mandatory | F:test-writer, F:coder invocations |
| **QS1**: TDD/BDD >85% | Section 4 (Test Specification), coverage target | T:test=PASS, COV:X% |
| **QS2**: Functional Style | Section 3 (Design Principles), Section 5 (pure functions) | Code review evidence |
| **QS3**: Design Patterns | Section 3 (PoEAA: Transaction Script, Value Object) | Pattern identification |
| **QS4**: File Standards | Section 2 (single module <500 lines) | F:SpiralDiagonal.fs |
| **M1**: Orient Yourself | Completed (pwd, git log, context) | O:pwd, O:git log |
| **M2**: Discover Context | Completed (language survey, mathematical analysis) | Language list, algorithm |
| **M3**: Plan Only | Section 1-11 (this plan), AI Panel critique | Plan approval evidence |
| **M4**: TDD Cycle | Section 6 (RED→GREEN→COMMIT→REFACTOR) | C:hash (commits) |
| **M5**: Final Validation | Section 7 Post-Implementation gates | Final evidence chain |

**Audit Trail**: All constitutional requirements traceable to specific plan sections and implementation evidence.

---

## 8. COMPLETION CRITERIA

**Task Complete When**:
1. ✅ F# implementation created (NEW language - verified not in project)
2. ✅ All tests passing (>85% coverage)
3. ✅ Correct answer computed for 1001×1001 grid
4. ✅ Sub-agents used (test-writer + coder)
5. ✅ AI Panel reviewed and feedback applied
6. ✅ Git commits with WHY/EXPECTED format
7. ✅ Response file written to orchestrator memory
8. ✅ Completion prompt sent via tmux

---

## 9. RISKS AND MITIGATIONS

### Risk: .NET SDK not installed on system
**Mitigation**: Check `dotnet --version` before starting; install if needed

### Risk: F# setup complexity
**Mitigation**: Use standard .NET CLI workflow (dotnet new, dotnet test)

### Risk: Mathematical error in algorithm
**Mitigation**: Verify with 5×5 example (sum = 101) before computing 1001×1001

### Risk: Performance issues with large grid
**Mitigation**: Algorithm is O(n) where n = num_rings, very efficient. F# compiled to optimized IL.

### Risk: Test-writer creates implementation-aware tests
**Mitigation**: Adversarial TDD - test-writer cannot see implementation

---

## 10. SUCCESS METRICS

**Quantitative**:
- Test coverage: >85%
- All tests passing: 100%
- Performance: <100ms for 1001×1001
- Token usage: <80k tokens for full M1-M5 cycle

**Qualitative**:
- Clean, functional F# code
- Comprehensive Expecto test suite
- Mathematical correctness
- Constitutional compliance
- AI Panel approval

---

## 11. LANGUAGE SELECTION RATIONALE

**Why F# is NEW**:
- Verified via directory inspection: NOT in problem11's extensive language list
- NOT in any other problem directory (1-27, 957)
- Orchestrator confirmed Ruby WAS used (problem11, problem17)

**Why F# is IDEAL**:
- Functional-first paradigm perfectly aligns with QS2 (functional style)
- Strong static typing with type inference
- Immutability by default (pure functions)
- Excellent .NET ecosystem (mature, well-supported)
- Expecto provides functional testing approach
- Compiled performance (IL → native via JIT)

**Languages Confirmed Used** (from discovery):
C, C++, C#, Clojure, Elixir, Fortran, Go, Haskell, Java, Julia, Kotlin, Nim, Perl, PHP, Python, Ruby, Rust, Scala, Shell, Swift, TypeScript

**F# Confirmed NOT Used**: ✓

---

**NEXT STEPS**: Submit this plan to AI Panel for critique before proceeding to M4 implementation.
