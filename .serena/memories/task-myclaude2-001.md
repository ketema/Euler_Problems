# TASK: Solve Project Euler Problems #31 and #32

## Task ID
task-myclaude2-001

## Orchestrator
claude-orchestrator (Claude Code instance)

## Agent
myclaude2

## Requirements
Solve Project Euler problems #31 and #32 with FULL CONSTITUTIONAL ADHERENCE.

### Problem #31: Coin Sums
In the United Kingdom the currency is made up of pound (£) and pence (p). There are eight coins in general circulation:
1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).

How many different ways can £2 be made using any number of coins?

### Problem #32: Pandigital Products
Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

Example: 39 × 186 = 7254 is pandigital (contains digits 1-9 exactly once when you write 391867254)

## Constitutional Requirements (MANDATORY)

### CL6 TDD ENFORCEMENT
- RED → GREEN → COMMIT → REFACTOR cycle (MANDATORY)
- Tests MUST be written FIRST
- Tests MUST describe behavior, not trivial checks
- Evidence: Test files, test output, commits

### Evidence Format (MANDATORY)
Use canonical compressed format:
```
F:path/to/file.py:10-50
T:module::test_name=PASS
C:commit_hash
COV:X%
O:"output snippet"
```

### Response Template (MANDATORY)
```
STATE: <current workflow state>
BRANCH: <git branch>
TOKEN_BUDGET: <current>/<total> (<percent>%) - <remaining> remaining
NEXT MACRO: <next deterministic macro>

ACTIONS:
1. ...

EVIDENCE:
...

BLOCKERS: <missing info or none>
```

### Completion Gates (CL2)
- All tests pass
- Coverage >85%
- Evidence logged
- Serena MEMORY updated
- Git commits with WHY/EXPECTED format

## Constraints
1. Activate Serena project: `Euler_Problems`
2. Follow M1→M2→M3→M4→M5 macros
3. Use Serena think tools at checkpoints
4. Store solutions in appropriate files
5. Provide correct numerical answers

## Deliverables
1. Solutions to both problems (#31, #32)
2. Test files with passing tests
3. Git commits with proper format
4. Evidence of constitutional compliance
5. Write response to: `.serena/memories/task-myclaude2-001-response.md`

## Evaluation Criteria
1. **Correctness**: Are the numerical answers correct?
2. **Constitutional Compliance Score**: Rules followed / Total applicable rules × 100%
   - CL1-CL9 (9 laws)
   - QS1-QS6 (6 standards)
   - M1-M5 (5 macros with mandatory checkpoints)
   - Evidence format compliance
   - Response template compliance

## Notification Protocol
When complete, send completion prompt to orchestrator:
```bash
tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: task-myclaude2-001. Problems #31, #32 solved. Read: task-myclaude2-001-response.md"
tmux send-keys -t claude-orchestrator.0 C-m
```

## Start Command
Activate Serena project first:
```
/serena activate Euler_Problems
```

Then begin with M1 ORIENT YOURSELF.
