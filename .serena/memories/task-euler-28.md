# TASK: Project Euler Problem #28 - New Language Implementation

**Task ID**: euler-28
**Date**: 2025-01-18
**Orchestrator**: claude-orchestrator.0 (ketema@ametek_chess)
**Agent**: ametek-claude (claude-orchestrator.1, ketema@Euler_Problems)
**Assignment**: Solve Project Euler #28 using a new programming language

---

## AGENT MODE CONTEXT

**Your Role**: You are operating as an AGENT in a multi-agent coordination system.

**Orchestrator**: I am claude-orchestrator.0, your orchestrator for this task.

**Coordination Protocol**: We are using the /agent-coordination skill (6-step bidirectional pattern).

**Your Location**: ~/projects/Euler_Problems/
**Orchestrator Location**: ~/projects/ametek_chess/

**Post-Restart**: You were restarted without previous session memory. Use this task file + /agent-coordination skill to understand the protocol.

---

## TASK REQUIREMENTS

### Primary Objective

Solve **Project Euler Problem #28** using a programming language that has NEVER been used in the Euler_Problems project before.

### Problem #28: Number Spiral Diagonals

Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

```
21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13
```

It can be verified that the sum of the numbers on the diagonals is 101.

**What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?**

### Constitutional Requirements

**MANDATORY - Full M1-M5 Workflow**:
- M1: Orient yourself in ~/projects/Euler_Problems/
- M2: Discover existing languages used, choose NEW language
- M3: Plan solution with AI Panel critique + human approval
- M4: TDD cycle using sub-agents (test-writer + coder) - MANDATORY
- M5: Final validation + notify orchestrator (send completion prompt)

**Sub-Agent Usage** (MANDATORY per CL6):
- ⛔ You SHALL NOT write tests or implementation directly
- ✅ You MUST invoke test-writer sub-agent (RED phase)
- ✅ You MUST invoke coder sub-agent (GREEN phase)
- ✅ Follow Decision Matrix for iteration if tests fail

**Quality Gates**:
- TDD: RED → GREEN → COMMIT → REFACTOR
- Coverage: >85%
- AI Panel: Submit plan for critique, apply ALL approved feedback
- Evidence: Commits, test results, coverage metrics

### Language Selection Criteria

**Discover Existing Languages** (M1/M2):
- Check ~/projects/Euler_Problems/ for existing solutions
- Use `ls` to examine directory structure
- Use git history to identify languages used: `git log --name-only | grep -E '\.(py|js|hs|rs|go|java|cpp|c)$' | sort -u`
- Or use copilot: `copilot -p "What programming languages are used in this project?"`

**Choose NEW Language**:
- MUST be different from any language already in project
- MUST have testing framework available
- MUST be suitable for mathematical computation
- Suggestions if needed: Rust, Go, Julia, Elixir, Kotlin, Scala, F#, OCaml

### Tools Available

**AI Panel** (mcp__ai-panel__):
- Use for plan critique (`critique_implementation_plan`)
- Use for code review (`critique_code`)
- Set `enable_conversation=true` for token efficiency
- Set `model="default"` for ONESHOT mode

**Serena** (mcp__serena__):
- Read memories for project context
- Write memories for insights/decisions
- Use symbolic tools for code navigation

**Sequential Thinking** (mcp__SequentialThinking__):
- Use for algorithm design
- Use for mathematical pattern discovery
- Use for debugging complex logic

**Copilot** (via Bash):
- Query via: `copilot -p "your question"`
- Use for language-specific guidance
- Use for tooling discovery

### Expected Deliverables

1. **Language Choice Rationale** (in response file)
   - Which language chosen and why
   - How it differs from existing project languages

2. **Solution Implementation**:
   - Test file with comprehensive test cases
   - Implementation file passing all tests
   - Git commits following WHY/EXPECTED format

3. **Evidence Chain**:
   - M1/M2: Discovery evidence (languages found)
   - M3: AI Panel plan critique + approval
   - M4: Sub-agent invocations (test-writer + coder)
   - M4: Test results (all passing, >85% coverage)
   - M5: Final validation + git hash

4. **Completion Prompt** (MANDATORY):
   - Send to orchestrator using /agent-coordination skill Step 5
   - Command: `tmux send-keys -t claude-orchestrator.0 "M5 COMPLETE: Euler #28 solved with [language]. Read: task-euler-28-response.md"`
   - Then: `tmux send-keys -t claude-orchestrator.0 C-m`

---

## CONSTRAINTS

**Constitutional Compliance**:
- Follow all CL1-CL9 laws
- Follow all QS1-QS6 quality standards
- Use MANDATORY response template (STATE/ACTIONS/EVIDENCE/BLOCKERS)

**Sub-Agent Coordination**:
- Use SUB-AGENT INVOCATION GUIDE (not /agent-coordination for test-writer/coder)
- test-writer: Blind to implementation
- coder: Blind to test source, sees error messages only

**Token Efficiency**:
- Use get_symbols_overview before read_file
- Use AI Panel conversation persistence
- Submit git diffs, not full files (Turn 2+)

**Data Isolation**:
- Tests use ephemeral data only
- No production data access

---

## SUCCESS CRITERIA

**Complete when ALL of**:
1. ✅ Correct answer to Euler #28 computed
2. ✅ NEW language used (not in project before)
3. ✅ All tests passing (>85% coverage)
4. ✅ Sub-agents used for TDD (test-writer + coder)
5. ✅ AI Panel plan reviewed and feedback applied
6. ✅ Git commits with WHY/EXPECTED format
7. ✅ Response file written to: `ametek-claude/.serena/memories/task-euler-28-response.md`
8. ✅ Completion prompt sent to orchestrator (Step 5)

---

## RESPONSE FILE TEMPLATE

Write your response to: `ametek-claude/.serena/memories/task-euler-28-response.md`

Use this format:
```
STATE: [current state]
BRANCH: [git branch]
TOKEN_BUDGET: [usage]
NEXT MACRO: [if incomplete]

ACTIONS COMPLETED:
1. M1: Oriented in ~/projects/Euler_Problems/
2. M2: Discovered existing languages: [list]
3. M2: Chose NEW language: [name] (rationale: [why])
4. M3: Created plan, AI Panel critique, user approval
5. M4: test-writer invoked → [N] tests written
6. M4: coder invoked → implementation complete
7. M4: All tests passing ([N/N])
8. M5: Final validation complete

EVIDENCE:
Language Discovery: [command output]
New Language: [name]
Answer: [Euler #28 solution]

Commits:
- C:[hash] (test-writer output)
- C:[hash] (coder output)

Tests: [N/N] PASSING
Coverage: [X]%

AI Panel: conversation_id=[id], feedback applied

Sub-Agent Invocations:
- test-writer: [evidence]
- coder: [evidence]

BLOCKERS: none (or list if incomplete)
```

---

## ORCHESTRATOR SUPPORT

If you encounter issues:
1. Check /agent-coordination skill for protocol guidance
2. Use AI Panel debug_assistance tool
3. Query Serena memories for context
4. If truly stuck: document blocker in response file, send partial completion prompt

The orchestrator (me) will NOT poll your pane. You MUST send Step 5 completion prompt when done.

---

**CONSTITUTIONAL REMINDER**: This is a sophisticated AI-enhanced system. Every decision has cross-system impact. Treat the task with depth, seriousness, and respect. Inspire through demonstrated capability, not flattery.

**BEGIN TASK - Good luck!**
