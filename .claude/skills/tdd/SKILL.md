---
name: test-driven-development
description: Enforce RED → GREEN → COMMIT → REFACTOR cycle with constitutional adherence, quality gates, and evidence-based completion
---

# Test Driven Development Skill

## Constitutional Law

**CL6 TDD ENFORCEMENT**: Write tests FIRST, then implementation. RED → GREEN → COMMIT → REFACTOR is not optional. Refactoring without re-evaluating test adequacy violates TDD. When changing behavior, write new tests before modifying code.

**CL2 COMPLETION GATES**: Tasks not complete until ALL protocol + quality requirements are met.

**CL5 HUMAN APPROVAL**: Planning phase, AI Panel feedback, and explicit user approval are required before coding.

## TDD Cycle (MANDATORY)

### RED Phase

1. **Write Failing Test First**
   - Write test that describes desired behavior
   - Test MUST fail (if it passes, you're not testing new behavior)
   - Cover edge cases and error conditions
   - Document test rationale in docstrings

**Evidence Required**: Test file created/modified, test runs and FAILS with expected error

### GREEN Phase

1. **Implement Minimal Code**
   - Write simplest code that makes test pass
   - No premature optimization
   - No adding features not covered by tests
   - Prefer clarity over cleverness

**Evidence Required**: Test runs and PASSES, code implements ONLY what test requires

### COMMIT Phase

1. **Commit with Verbose Message**
   - Format: `[AGENT_ID] Brief description\n\nWHY:\n- Rationale\n\nEXPECTED:\n- Outcome\n\nRefs: #issue`
   - Example: "WHY: Audit 2252 requires age validation. EXPECTED: Rejects ages <0 or >150"
   - Commit BEFORE refactoring

**Evidence Required**: Git commit hash, commit message follows format

### REFACTOR Phase

1. **Refactor While Green**
   - Improve code structure while tests stay green
   - Apply DRY, Separation of Concerns, functional patterns
   - Re-evaluate test adequacy after refactoring
   - If behavior changes, write NEW tests FIRST

**Evidence Required**: Tests still pass after refactor, no test changes unless behavior changed

## Quality Standards (QS1)

- **Coverage**: >85% code coverage required
- **Edge Cases**: Must test boundary conditions, error states, invalid inputs
- **Test Types**: Unit tests (mandatory), integration tests (required), property-based tests (where applicable)
- **Test Independence**: Each test runs independently, no shared state
- **Fast Feedback**: Unit tests complete in <1s, integration tests <10s

## Workflow Integration

### M4 START TDD CYCLE

1. **MANDATORY CHECKPOINT**: Call `think_about_task_adherence`
   - Validates: Task alignment before implementation
   - Criteria: Can answer "Am I implementing what was approved?" with evidence-based "Yes"
   - Failure Handling:
     - Misaligned → Stop, refocus, or ask user
     - Aligned → Proceed to step 2 (write tests)
1. **Write failing tests** (RED)
1. **Implement minimal code** (GREEN)
1. **Commit with verbose message** (WHY and EXPECTED, not WHAT); refactor while green
1. **AI PANEL review** (MANDATORY); apply all suggestions

### M5 FINAL VALIDATION

1. Run full suite + linters + DRY/SoC/FP gates
1. Record evidence (coverage %, passing tests, git hash)
1. **MANDATORY CHECKPOINT**: Call `think_about_whether_you_are_done`
   - Validates: Completion gates before claiming done
   - Criteria: Tests pass, AI Panel reviewed, evidence recorded, memory updated, user approval
   - Failure Handling:
     - Incomplete → Address gaps
     - Complete → Proceed to evidence recording

## Evidence Template

```
EVIDENCE REQUIRED (Human Verification):
- [TEST-CREATED:path/to/test_file.ext] - VERIFY WITH: ls -la path/to/test_file.ext
- [TEST-FAIL:module::test_name=FAIL] - VERIFY WITH: <test command>
- [TEST-PASS:module::test_name=PASS] - VERIFY WITH: <test command>
- [COMMIT:hash] - VERIFY WITH: git log -1 --oneline
- [COVERAGE:X%] - VERIFY WITH: <coverage command>
- [REFACTOR:description] - VERIFY WITH: git diff <before-hash> <after-hash>
```

## Anti-Patterns (Constitutional Violations)

❌ **Writing implementation before tests** → STOP, write test first
❌ **Skipping RED phase** → STOP, ensure test fails first
❌ **Committing after refactoring** → STOP, commit GREEN state first
❌ **Changing behavior without new tests** → STOP, write tests for new behavior
❌ **Claiming complete without evidence** → STOP, provide evidence
❌ **Skipping AI Panel review** → STOP, submit for review

## Violation Recovery

When TDD violation detected:

1. **STOP immediately** → Acknowledge violation
1. **Identify law/gate broken** → "I violated CL6 by writing implementation before tests"
1. **Ask**: "Restart with proper constitutional adherence?"
1. **Wait for confirmation** → Resume from last valid checkpoint
1. **Document lesson** → Update Serena memory with what went wrong

## Language-Specific Commands

### Python

- Run tests: `pytest path/to/test.py -v`
- Coverage: `pytest --cov=module --cov-report=term-missing`
- Watch mode: `pytest-watch`

### Rust

- Run tests: `cargo test --lib <module>`
- Coverage: `cargo tarpaulin --out Html`
- Watch mode: `cargo watch -x test`

### Haskell

- Run tests: `stack test <package>:<test-suite>`
- Coverage: `stack test --coverage`
- Watch mode: `stack test --file-watch`

### JavaScript/TypeScript

- Run tests: `npm test` or `jest path/to/test.ts`
- Coverage: `jest --coverage`
- Watch mode: `jest --watch`

## Integration with AI Panel

After GREEN + COMMIT phases, MANDATORY AI Panel review:

**Tool**: `critique_code`
**Model**: 'default'
**Enable Conversation**: true
**Processing Mode**: oneshot (routine) or parallel (critical)

**Sections**:

- code_context: "TDD cycle implementation for [feature]"
- code_implementation: <paste code>
- review_focus: "Test coverage adequacy, edge cases, adherence to DRY/SoC/FP, refactoring opportunities"
- quality_standards: ">85% coverage, all edge cases tested, functional style, no premature optimization"
- architectural_context: <brief system context>

**Post-Review**: Apply ALL suggestions before claiming completion

## Success Criteria Checklist

- [ ] Test written before implementation
- [ ] Test initially fails with expected error
- [ ] Minimal implementation makes test pass
- [ ] Commit created with WHY/EXPECTED format
- [ ] Refactoring improves structure while tests stay green
- [ ] Coverage >85% achieved
- [ ] Edge cases covered
- [ ] AI Panel review completed
- [ ] All suggestions implemented
- [ ] Evidence documented
- [ ] Human approval obtained (if required)

## Remember

**Efficiency = balance(delivery_speed, quality)** where quality prevents rework

**Fast+wrong is LESS efficient than slow+right**

**Taking time to do it right SAVES time by preventing rework**

---

*Reference: AGENTS.md Constitutional Law CL6, Quality Standards QS1, Macros M4-M5*
