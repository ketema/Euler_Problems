# Context Handoff: Multi-Agent Orchestrator Session

## Session Status: READY FOR AGENT ORCHESTRATION

**Current Branch**: master  
**Token Budget**: ~65k/200k used (33%)  
**Last Commit**: 7ae1db1 (environment configuration analysis)

---

## CRITICAL CONTEXT

### Primary Mission (Next Session)
**User Intent**: "you will be orchestrating 2 agents: Gemini and myclaude2 (another instance of yourself) I will give details in the new session, but basically you will be guiding them through solving euler problems simultaneously as an exercise of the agent orchestration protocol."

**Your Role**: **ORCHESTRATOR**  
**Agents to Coordinate**: 
- gemini (worktree: `euler-gemini/`)
- myclaude2 (worktree: `euler-myclaude2/`)

**Task**: Guide both agents through solving Euler problems simultaneously as exercise of agent coordination protocol

**Protocol Reference**: `/agent-coordination` skill (6-step bidirectional pattern)

---

## Infrastructure Status: COMPLETE ✅

### Worktree Setup (Completed This Session)
- ✅ Created `euler-gemini/` worktree with isolated environment
- ✅ Created `euler-myclaude2/` worktree with isolated environment
- ✅ Both worktrees have agent-specific .envrc files
- ✅ Build isolation configured (Rust, Python, Haskell, Node, F#)
- ✅ Cross-project agent-coordination skill updated (uses ${AGENT_WORKTREE_PREFIX})

### Worktree Manager Script
**Location**: `scripts/worktree_manager.sh` (814 lines)
**Capabilities**:
- Create agent worktrees: `./scripts/worktree_manager.sh create <agent-name>`
- Remove agent worktrees: `./scripts/worktree_manager.sh remove <agent-name>`
- List worktrees: `./scripts/worktree_manager.sh list`
- Sync .envrc: `./scripts/worktree_manager.sh sync-envrc`

**Key Adaptations from ametek_chess**:
- Removed database functions (Euler has no persistent data)
- Removed port generation (no listening services)
- Changed prefix: ametek-* → euler-*
- Added F# support (FSHARP_NUGET_DIR)

### Environment Configuration
**.envrc Template**: `scripts/templates/envrc.template`
**Main .envrc**: Project root (sets AGENT_WORKTREE_PREFIX="euler")

**Build Isolation Variables** (per agent):
```bash
CARGO_TARGET_DIR="target-<agent>"
PYTHON_VENV_DIR=".venv-<agent>"
HASKELL_STACK_WORK=".stack-work-<agent>"
NODE_MODULES_DIR="node_modules-<agent>"
FSHARP_NUGET_DIR=".nuget-<agent>"
```

---

## Language Inventory

**Current Unique Languages in Use**: **22** (corrected: 21 programming languages, markdown excluded)

**Active Languages**:
1. c, 2. cpp, 3. csharp, 4. elixir, 5. fortran, 6. fsharp, 7. go, 8. haskell, 9. java, 10. javascript, 11. julia, 12. kotlin, 13. nim, 14. perl, 15. php, 16. python, 17. ruby, 18. rust, 19. scala, 20. shell, 21. swift, 22. typescript

**Serena Configured**: 27 languages (includes markdown which won't be used for problems)

**Build Isolation Gaps**: Java, Go, Ruby, Scala, Kotlin, Swift, C/C++, Nim need .envrc extensions when heavily used  
**Reference**: `.serena/memories/polyglot-build-isolation-pattern.md`

---

## Agent Coordination Protocol (CRITICAL)

### When to Invoke `/agent-coordination` Skill
**You are the ORCHESTRATOR**. You will use Steps 1-3 and Step 6 from the protocol.

### Orchestrator Steps (Your Responsibilities)

**Step 1: Write Task**
- Create `.serena/memories/task-[id].md` with:
  ```yaml
  task_description: "Solve Euler Problem #X using [language]"
  requirements: |
    REQ-EULER-XXX: [Problem statement]
  constraints:
    - Use TDD (RED→GREEN→COMMIT→REFACTOR)
    - Follow constitutional macros (M1-M5)
    - Document solution in README
  context:
    - Agent: [gemini|myclaude2]
    - Worktree: euler-[agent]/
    - Problem: #X
  ```

**Step 2: Send Prompt via tmux**
```bash
# For gemini (pane 1):
tmux send-keys -t claude-orchestrator.1 "TASK: Solve Euler #X. Read: task-[id].md"
tmux send-keys -t claude-orchestrator.1 C-m

# For myclaude2 (pane 2):
tmux send-keys -t claude-orchestrator.2 "TASK: Solve Euler #X. Read: task-[id].md"
tmux send-keys -t claude-orchestrator.2 C-m
```

**Step 3: Verify Delivery (ONCE)**
```bash
# Check pane 1 (gemini):
tmux capture-pane -t claude-orchestrator.1 -p | tail -20

# Check pane 2 (myclaude2):
tmux capture-pane -t claude-orchestrator.2 -p | tail -20
```
**CRITICAL**: NO POLLING between Step 3 and Step 6. Trust memory-based handoff.

**Step 6: Read Response & Audit**
- Wait for agent completion prompt in orchestrator pane (pane 0)
- Read response from `euler-[agent]/.serena/memories/task-[id]-response.md`
- Audit compliance (tests passing, commits made, README documented)
- Provide feedback or approve

### Agent Steps (What Agents Will Do - You Don't Execute These)
**Step 4**: Agent reads task, executes M1→M5, writes response  
**Step 5**: Agent sends completion prompt back to orchestrator pane

### Pane Mapping (Assumed tmux Session)
- **Pane 0**: claude-orchestrator.0 (YOUR pane - orchestrator)
- **Pane 1**: claude-orchestrator.1 (gemini agent)
- **Pane 2**: claude-orchestrator.2 (myclaude2 agent)

**Note**: User may configure differently - verify pane targets in next session

---

## Memory Paths (Critical for Protocol)

**Task Assignment** (you write):
- `.serena/memories/task-[id].md` (main repo)

**Agent Responses** (agents write):
- `euler-gemini/.serena/memories/task-[id]-response.md`
- `euler-myclaude2/.serena/memories/task-[id]-response.md`

**Shared Memory** (cross-agent institutional knowledge):
- `$HOME/.config/claude-code/shared-memory.json` (set in .envrc)

---

## Constitutional Framework Refresher

**Your Role as Orchestrator**:
- You DO NOT solve Euler problems directly
- You DELEGATE via agent-coordination protocol
- You AUDIT agent outputs for constitutional compliance
- You GUIDE agents when they deviate or get stuck

**Agent Responsibilities** (what you'll be monitoring):
- M1: Orient (pwd, git status, memory discovery)
- M2: Discover context (understand problem)
- M3: Plan (with AI Panel critique, human approval)
- M4: TDD cycle (test-writer → coder → iteration)
- M5: Final validation (tests pass, AI Panel reviewed, memory updated)

**Quality Gates You'll Enforce**:
- Tests passing (>85% coverage)
- AI Panel reviewed (critique_implementation_plan, critique_code)
- Git commits with WHY/EXPECTED format
- README documentation
- No constitutional violations (DRY, TDD, YAGNI, security)

---

## Token Efficiency Strategy

**Protocol Token Savings**:
- 6-step pattern with one verification: ~1,200 tokens per task
- Alternative (polling every 2 min): ~3,000+ tokens
- **Savings**: 60% per task

**When Coordinating 2 Agents Simultaneously**:
- Parallel task assignment (both agents work concurrently)
- No polling → wait for completion prompts
- Token budget: ~2,500 tokens for dual coordination vs ~6,000+ with polling
- **Savings**: ~58% for parallel orchestration

---

## Known Issues & Maintenance

**Polyglot Extensibility** (CRITICAL):
- As agents use new compiled languages, update `scripts/templates/envrc.template`
- Sync worktrees: `./scripts/worktree_manager.sh sync-envrc`
- Document new patterns in `.serena/memories/polyglot-build-isolation-pattern.md`

**Languages Likely to Need Build Isolation**:
- Java (when used): `.gradle/`, `.m2/`
- Go (when used): `GOPATH`, `GOCACHE`
- Ruby (when used): `GEM_HOME`, `BUNDLE_PATH`
- Scala (when used): `.sbt/`, `.ivy2/`

**Nim** (already in use):
- Not in Serena's supported list
- Requires manual configuration or alternative tooling
- `nimcache/` directory needs isolation if heavily used

---

## Workflow for Next Session

**Expected User Prompt**: User will provide details on which Euler problems to assign and orchestration parameters.

**Your Immediate Actions**:
1. **Invoke `/agent-coordination` skill** to refresh protocol
2. **Verify tmux session exists**: Check pane targets for gemini and myclaude2
3. **Read user's task specification**: Which problems? Any constraints?
4. **Create task-[id].md files**: One per agent assignment
5. **Send prompts via tmux**: Deliver tasks to both agents
6. **Verify delivery once**: Confirm both agents received prompts
7. **STOP POLLING**: Trust memory-based protocol
8. **Wait for completion prompts**: Agents will send "TASK COMPLETE" messages to your pane
9. **Read responses**: Audit agent outputs for compliance
10. **Provide feedback**: Approve or request revisions

**Constitutional Checkpoints You'll Monitor**:
- M1.3.5: think_about_collected_information (did agent gather sufficient context?)
- M3.4.5: think_about_task_adherence (is plan aligned with Euler problem requirements?)
- M4.1: think_about_task_adherence (is implementation following approved plan?)
- M5.3.5: think_about_whether_you_are_done (are completion gates satisfied?)

**AI Panel Usage You'll Verify**:
- M3: critique_implementation_plan (before coding starts)
- M4: check_plan_adherence + critique_code (during/after implementation)
- M5: critique_code (final validation)
- All with `enable_conversation=true` for institutional memory

---

## Quick Reference Commands

**List Active Worktrees**:
```bash
./scripts/worktree_manager.sh list
```

**Check Agent Pane Output**:
```bash
tmux capture-pane -t claude-orchestrator.1 -p | tail -20  # gemini
tmux capture-pane -t claude-orchestrator.2 -p | tail -20  # myclaude2
```

**View Agent Response**:
```bash
cat euler-gemini/.serena/memories/task-001-response.md
cat euler-myclaude2/.serena/memories/task-001-response.md
```

**Sync .envrc After Template Changes**:
```bash
./scripts/worktree_manager.sh sync-envrc
```

---

## State Assertion

**Infrastructure**: ✅ COMPLETE (worktrees created, environments isolated, protocol adapted)  
**Documentation**: ✅ COMPLETE (polyglot patterns, language inventory, build isolation documented)  
**Next Task**: ORCHESTRATE DUAL AGENTS (gemini + myclaude2) solving Euler problems  
**Readiness**: 100% - All prerequisites satisfied

**No Blockers**: Infrastructure is production-ready for dual-agent orchestration exercise.

---

## Final Notes

**User Correction**: Markdown is not a programming language → true language count is **21** (not 22)

**User Intent Confirmation**: You will be **orchestrating** (not solving directly). Your job is coordination, auditing, and guidance.

**Protocol Compliance**: Use `/agent-coordination` skill as primary reference. This memory is supplementary context.

**Token Budget Awareness**: Current usage ~65k/200k (33%). Plenty of headroom for orchestration work. Use token-efficient protocol (no polling).

**YAGNI Reminder**: Infrastructure copied+adapted for 2 projects (ametek_chess, Euler_Problems). When 3rd project emerges, abstract shared utilities. For now, maintain separate implementations.

---

**READY FOR ORCHESTRATION SESSION**
