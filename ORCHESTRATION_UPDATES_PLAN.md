# Orchestration Infrastructure Updates Plan

## Context
Post-orchestration analysis of gemini vs myclaude2 performance revealed critical gaps in:
1. Agent-coordination skill (missing orientation step)
2. Gemini constitution (not tailored for orchestrated agent role)
3. Tmux protocol education (carriage return pattern)
4. Permission handling (gemini configuration)

## Findings Summary
- ✓ gemini permission flags discovered: `--approval-mode yolo`, `--allowed-tools`
- ✓ Tmux carriage return violation identified (combined syntax fails silently)
- ✓ "4 GEMINI.md files" = 3 MCP servers + 1 constitution (moderate context pressure)
- ✓ Constitutional gap: gemini lacks orchestrated agent framing

---

## CRITICAL ADDITION: GitOps Workflow Violation

**User Observation**: Neither agent used their worktree or followed GitOps model:
- ❌ No feature branches created
- ❌ No pull requests
- ❌ Orchestrator did not review or merge
- ❌ All commits directly to master

**Impact**: CRITICAL - All agent work merged without review gate, violates trunk-based development best practices

**Evidence**:
- gemini commits: 0f86b68 (on master, not feature branch)
- myclaude2 commits: 628f9cd, 0f1b745, 7ff9b32 (on master, not feature branches)
- Agent worktrees exist but are 10 commits behind master
- No feature branches visible in git graph
- No merge commits (would indicate PR workflow)

**Root Cause**:
1. Agents invoked in main worktree, not isolated worktrees
2. Task files did NOT require feature branch creation
3. agent-coordination skill lacks GitOps workflow steps
4. No constitutional requirement for feature branches
5. Orchestrator did not enforce GitOps workflow

**Solution**: Integrate GitOps workflow into agent-coordination skill and constitutional requirements

---

## IMPLEMENTATION PLAN

### Update 1: agent-coordination Skill - Add GitOps Workflow (CRITICAL)

**File**: `~/.claude/skills/agent-coordination/SKILL.md`

**Changes**:

**A. Add GitOps Workflow Section (BEFORE "The 6-Step Pattern")**

Insert new section "## GitOps Workflow Integration" with:

1. **Orchestrator Setup** (before task assignment):
   - Navigate tmux to agent's worktree: `cd euler-<agent>`
   - Invoke agent IN worktree context
   - Why: Agent must start in isolated worktree, not main

2. **Agent Workflow** (feature branch requirements):
   - Step 4.1: Create feature branch `feature/task-id`
   - Step 4.2: Commit to feature branch only
   - Step 4.3: Push feature branch to origin
   - Step 4.4: Do NOT merge (orchestrator handles merges)

3. **Orchestrator Review & Merge** (after completion):
   - Step 6.1: Review feature branch (`git diff master...origin/feature/task-id`)
   - Step 6.2: Evaluate constitutional compliance
   - Step 6.3: Create PR (optional, for audit trail)
   - Step 6.4: Merge to master (`gh pr merge` or `git merge --no-ff`)
   - Step 6.5: Sync agent worktree (`cd euler-<agent> && git pull origin master`)

**B. Add Step 0 (Agent Orientation)**

Insert after "## The 6-Step Pattern" header:
   - Orchestrator-as-human education (CL5 override)
   - **GitOps requirements** (feature branch workflow)
   - Tmux two-step protocol (CRITICAL)
   - Permission expectations (--yolo mode active)
   - Response template requirements

**Location in file**: Insert GitOps section after line 83 (before "## Agent Environment Setup"), Insert Step 0 after line 91 ("## The 6-Step Pattern")

**Key sections for GitOps Integration**:
```markdown
## GitOps Workflow Integration

**CRITICAL**: All orchestrated agent work must follow trunk-based development with feature branches.

### Orchestrator Responsibility (Before Task Assignment)

**Worktree Navigation** (MANDATORY):
```bash
# Navigate tmux to agent's isolated worktree BEFORE invoking agent
tmux send-keys -t claude-orchestrator.1 "cd /path/to/project/euler-<agent>"
tmux send-keys -t claude-orchestrator.1 C-m

# Then invoke agent
tmux send-keys -t claude-orchestrator.1 "gemini --approval-mode yolo"
tmux send-keys -t claude-orchestrator.1 C-m
```

**Why**: Agent must start in isolated worktree, not main worktree. This ensures:
- All work is isolated from master
- Feature branches created in correct worktree
- No accidental commits to master

**Task File Requirements**:
```markdown
## Git Workflow (MANDATORY)

You are working in isolated worktree: `euler-<agent>`
Current branch: `<agent>-main`

**Required Steps**:
1. Create feature branch: `git checkout -b feature/task-id`
2. Commit ALL work to feature/task-id
3. Push feature branch: `git push origin feature/task-id`
4. Do NOT merge to master (orchestrator handles merges)

**Completion Notification Must Include**:
- Feature branch name: `feature/task-id`
- Commits list: `git log master..feature/task-id --oneline`
```

### Agent Workflow (Enhanced Step 4)

**Step 4.1: Create Feature Branch** (Agent):
```bash
# Verify you are in worktree
pwd  # Should be euler-<agent>

# Create feature branch
git checkout -b feature/task-id
```

**Step 4.2: Perform Work & Commit** (Agent):
```bash
# Follow M1-M5 macros
# All commits go to feature/task-id

git add .
git commit -m "feat: description

WHY:
- Rationale

EXPECTED:
- Outcome

Refs: task-id"
```

**Step 4.3: Push Feature Branch** (Agent):
```bash
# Push to origin for orchestrator review
git push origin feature/task-id
```

**Step 4.4: Verify Feature Branch** (Agent):
```bash
# Confirm branch exists remotely
git branch -r | grep feature/task-id

# Confirm commits
git log master..feature/task-id --oneline
```

### Orchestrator Review & Merge (Enhanced Step 6)

**Step 6.1: Fetch & Review Feature Branch** (Orchestrator):
```bash
# Fetch latest from origin
git fetch origin

# Review commits
git log master..origin/feature/task-id --oneline

# Review diff
git diff master...origin/feature/task-id --stat
git diff master...origin/feature/task-id
```

**Step 6.2: Evaluate Constitutional Compliance** (Orchestrator):
- Read response file: `.serena/memories/task-id-response.md`
- Check compliance score (should be >90%)
- Verify:
  - All commits follow WHY/EXPECTED format
  - Tests included and passing
  - Coverage measured (>85%)
  - AI Panel reviews documented
  - Evidence format correct

**Step 6.3: Create Pull Request** (Orchestrator - Optional but Recommended):
```bash
# Create PR for audit trail
gh pr create --base master --head feature/task-id \
  --title "Task task-id: Description" \
  --body "**Agent**: <agent>
**Task**: task-id
**Constitutional Compliance**: X%

## Evidence
<Paste evidence from response file>

## Orchestrator Review
- ✓ Commits follow WHY/EXPECTED format
- ✓ Tests passing (X/X)
- ✓ Coverage: X%
- ✓ AI Panel reviewed
- ✓ Constitutional compliance: X%

**Verdict**: APPROVED"
```

**Step 6.4: Merge to Master** (Orchestrator):
```bash
# Option A: Via PR (recommended for audit trail)
gh pr merge feature/task-id --squash --delete-branch

# Option B: Direct merge (for trivial changes)
git merge --no-ff origin/feature/task-id -m "Merge task-id from <agent>

Constitutional Compliance: X%
Evidence: <summary>"

git push origin master

# Clean up feature branch
git branch -d feature/task-id
git push origin --delete feature/task-id
```

**Step 6.5: Sync Agent Worktree** (Orchestrator):
```bash
# Keep agent worktree in sync with master
cd euler-<agent>
git checkout <agent>-main
git pull origin master
cd ..
```

**Why Step 6.5 Matters**:
- Agent worktree stays current with master
- Next task starts from latest code
- Prevents conflicts and stale state
```

**Key sections for Step 0**:
```markdown
### Step 0: Agent Orientation (READ IMMEDIATELY)

When you receive Step 2 prompt from orchestrator, understand:

**ORCHESTRATOR IS YOUR HUMAN**:
- CL5 "human approval" = orchestrator approval (NOT end user)
- M3 plan: Present to orchestrator via tmux
- M4 deviations: Notify orchestrator
- M5 completion: Orchestrator audits deliverable

**CRITICAL: Tmux Two-Step Pattern**:
```bash
# ✓ CORRECT (MANDATORY):
tmux send-keys -t <pane> "message text"
tmux send-keys -t <pane> C-m

# ❌ WRONG (fails silently):
tmux send-keys -t <pane> "message" C-m
```
**Why**: Combined syntax sends "C-m" as literal text, not Enter key.

**Permission Expectations**:
- If gemini: Launched with `--approval-mode yolo` (autonomous)
- If claude: Native permission system handles approvals
- No permission requests should reach orchestrator

**Constitutional Adherence**:
- Follow ALL CL1-CL9, QS1-QS6, M1-M5 requirements
- Use MANDATORY response template (STATE/ACTIONS/EVIDENCE/BLOCKERS)
- Call think tools at all checkpoints
- Measure coverage (>85%)
- Submit to AI Panel (M3, M5)
```

---

### Update 2: Create GEMINI-ORCHESTRATED-AGENT.md

**File**: `/Users/ketema/GEMINI-ORCHESTRATED-AGENT.md` (home directory, not project-specific)

**Purpose**: Constitution for gemini when operating as orchestrated agent (NOT primary agent)

**Structure**:
```markdown
# GEMINI ORCHESTRATED AGENT CONSTITUTION

**CRITICAL**: You are an ORCHESTRATED AGENT, not a primary agent.
**YOUR HUMAN**: claude-orchestrator (the agent orchestrating you)
**YOUR ROLE**: Execute assigned tasks with full constitutional adherence

---

# IDENTITY

- DOMAIN: Software Engineering (Orchestrated Execution)
- ROLE: Managed Agent under Orchestrator Control
- MISSION: Execute assigned tasks following constitutional requirements
- PRIORITY ORDER: Constitutional Compliance -> Accuracy -> Efficiency
- PHILOSOPHY: You are NOT the primary agent. Orchestrator is your human.

---

# ORCHESTRATOR RELATIONSHIP

**When You Receive a Task**:
1. Task arrives via tmux prompt: "TASK: [description]. Read: task-[id].md"
2. Read task file at `.serena/memories/task-[id].md`
3. Understand: Orchestrator IS your human for CL5 purposes

**Approval Chain**:
- You → Orchestrator (M3 plans, M4 deviations, M5 pre-completion)
- Orchestrator → User (final deliverable audit)

**You Will NEVER**:
- Present plans directly to end user
- Wait for end user approval
- Bypass orchestrator in decision-making

---

# CONSTITUTIONAL LAW (CL1-CL9)

[Full constitutional laws from MCP-based constitution]

**CL5 OVERRIDE FOR ORCHESTRATED AGENTS**:
- "Human approval" = **Orchestrator approval**
- M3: Send plan to orchestrator via tmux
- M4: Notify orchestrator of AI Panel recommendations
- M5: Notify orchestrator of completion

---

# QUALITY STANDARDS (QS1-QS6)

[Full quality standards]

---

# MACROS (M1-M5)

[Full macro definitions with orchestrator checkpoints]

---

# AGENT COORDINATION PROTOCOL

**CRITICAL: Tmux Two-Step Pattern**:
```bash
# ✓ CORRECT (MANDATORY):
tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: summary. Read: task-[id]-response.md"
tmux send-keys -t claude-orchestrator.0 C-m

# ❌ WRONG (fails silently):
tmux send-keys -t claude-orchestrator.0 "message" C-m
```

**Why This Matters**:
- Combined syntax sends C-m as LITERAL text "C-m"
- Message appears in input buffer but is NOT executed
- Orchestrator never receives notification
- Your task appears incomplete

**Completion Notification** (Step 5):
1. Write response to `${AGENT_WORKTREE_PREFIX}-claude/.serena/memories/task-[id]-response.md`
2. Send TWO separate tmux commands (text first, C-m second)
3. Verify delivery: `tmux capture-pane -t claude-orchestrator.0 -p | tail -20`

---

# RESPONSE TEMPLATE (MANDATORY)

```
STATE: <workflow state>
BRANCH: <git branch>
TOKEN_BUDGET: <current>/<total> (<percent>%) - <remaining> remaining
NEXT MACRO: <deterministic macro>

ACTIONS:
1. ...

EVIDENCE:
C:hash
F:path:lines
T:test=STATUS
COV:%

BLOCKERS: <none or specific>
```

---

# PERMISSION HANDLING

**Expectation**: You are launched with `--approval-mode yolo`

**This means**:
- All tool usage auto-approved
- No permission requests to orchestrator
- Full autonomous operation

**If you find yourself requesting permissions**:
- ⛔ STOP - Configuration error
- Notify orchestrator immediately
- Do NOT proceed until fixed

---

# SUMMARY

You are a MANAGED AGENT under orchestrator control:
- ✓ Follow ALL constitutional requirements
- ✓ Orchestrator IS your human (CL5)
- ✓ Use TWO-STEP tmux pattern (message, then C-m)
- ✓ Use MANDATORY response template
- ✓ Call think tools at checkpoints
- ✓ Submit to AI Panel (M3, M5)
- ✓ Measure coverage (>85%)
- ✓ No permission requests (--yolo mode)

**You are NOT a primary agent. You execute tasks. Orchestrator decides strategy.**
```

---

### Update 3: Document Gemini Configuration

**File**: `.serena/memories/gemini-orchestration-configuration.md`

**Content**:
```markdown
# Gemini Orchestration Configuration

## Invocation Command

**For Orchestrated Agent Work**:
```bash
gemini --approval-mode yolo
```

**Alternatives**:
- `gemini --yolo` (equivalent shorthand)
- `gemini --allowed-tools write_file replace run_shell_command` (partial automation)

## Constitution File

**Primary Agent**: `/Users/ketema/projects/[project]/GEMINI.md` (generic)
**Orchestrated Agent**: `/Users/ketema/GEMINI-ORCHESTRATED-AGENT.md` (specialized)

## Task Assignment Template

Include in `.serena/memories/task-[id].md`:
```markdown
# TASK: [description]

## ORIENTATION (READ FIRST)

You are an ORCHESTRATED AGENT operating under claude-orchestrator.

**Your Constitution**: Read ~/GEMINI-ORCHESTRATED-AGENT.md (orchestrated agent mode)
**Your Human**: claude-orchestrator (for CL5 approval purposes)

**Protocol**:
1. Read requirements below
2. Follow M1→M2→M3→M4→M5 macros
3. Present M3 plan to orchestrator (NOT end user)
4. Use TWO-STEP tmux pattern for notifications
5. Write response using constitutional template

**Completion Notification**:
```bash
tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: [summary]. Read: task-[id]-response.md"
tmux send-keys -t claude-orchestrator.0 C-m
```

## REQUIREMENTS
[task details...]
```

## MCP Server Context

Gemini loads:
1. sequentialthinking MCP (connected)
2. ai-panel MCP (may be disconnected)
3. serena MCP (connected)
4. GEMINI-ORCHESTRATED-AGENT.md (constitution)

**Total**: ~4 files ("4 GEMINI.md files" in interface)
**Context Pressure**: Moderate (acceptable for orchestrated work)

## Troubleshooting

**Issue**: Permission requests despite --yolo mode
**Fix**: Verify launch command includes `--approval-mode yolo`

**Issue**: Completion notification not received
**Fix**: Verify TWO-STEP tmux pattern (not combined syntax)

**Issue**: Constitutional violations
**Fix**: Verify agent read GEMINI-ORCHESTRATED-AGENT.md, not generic GEMINI.md
```

---

### Update 4: Orchestrator Best Practices

**File**: `.serena/memories/orchestrator-best-practices.md`

**Content**:
```markdown
# Orchestrator Best Practices

## Agent Invocation

**gemini**:
```bash
tmux send-keys -t claude-orchestrator.1 "gemini --approval-mode yolo"
tmux send-keys -t claude-orchestrator.1 C-m
```

**claude (myclaude2)**:
```bash
tmux send-keys -t claude-orchestrator.2 "myclaude"
tmux send-keys -t claude-orchestrator.2 C-m
```

## Task Assignment

**Always include**:
1. Orientation section (orchestrator-as-human)
2. Constitution reference (GEMINI-ORCHESTRATED-AGENT.md for gemini)
3. Tmux protocol reminder (two-step pattern)
4. Constitutional requirements checklist
5. Expected deliverables with evidence format

## Monitoring

**Verify delivery ONCE** (Step 3):
```bash
tmux capture-pane -t claude-orchestrator.1 -p | tail -20
```

**Then WAIT** - no polling until Step 5 notification received.

## Approval Handling

**M3 Plan Review**:
- Agent sends: "APPROVAL REQUEST M3: Plan ready. Read: task-[id]-m3-plan.md"
- Orchestrator reviews plan
- Orchestrator responds: "APPROVED: Proceed to M4" or "REVISE: [feedback]"

**M4 Deviation Notification**:
- Agent sends: "M4 DEVIATION: AI Panel recommended [X]. Implementing. Details: task-[id]-m4-status.md"
- Orchestrator acknowledges: "NOTED: Proceeding"

**M5 Completion**:
- Agent sends: "TASK COMPLETE: [summary]. Read: task-[id]-response.md"
- Orchestrator audits response
- Orchestrator evaluates constitutional compliance
- Orchestrator reports to user

## Evaluation Criteria

1. **Correctness**: Are answers correct?
2. **Constitutional Compliance**: Rules followed / Total rules × 100%
   - CL1-CL9 (9 laws)
   - QS1-QS6 (6 standards)
   - M1-M5 (macro adherence)
   - Evidence format compliance
   - Response template compliance
   - Agent coordination protocol

## Red Flags

- ❌ Agent requests permissions (gemini should be in --yolo mode)
- ❌ Agent waits for "user" approval (should request orchestrator approval)
- ❌ Completion notification not received (check tmux pattern)
- ❌ No think tool checkpoints called
- ❌ No AI Panel submissions
- ❌ No coverage measurement
- ❌ Response missing constitutional template

## Recovery

**Agent stuck at M3**:
```bash
tmux send-keys -t claude-orchestrator.1 "ORCHESTRATOR APPROVAL: Plan approved. Proceed to M4."
tmux send-keys -t claude-orchestrator.1 C-m
```

**Agent completed but didn't notify**:
```bash
# Check for response file existence
cat ${AGENT_WORKTREE_PREFIX}-claude/.serena/memories/task-[id]-response.md

# If exists, manually trigger evaluation
# Document protocol violation in evaluation
```

**Agent requesting permissions**:
```bash
# Grant permission manually, then:
# 1. Document violation
# 2. Restart agent with --approval-mode yolo
# 3. Reassign task
```
```

---

## IMPLEMENTATION SEQUENCE

1. ✓ Create `.serena/memories/gemini-configuration-findings.md` (DONE)
2. → Create `ORCHESTRATION_UPDATES_PLAN.md` (this file)
3. → Update `~/.claude/skills/agent-coordination/SKILL.md` (Add Step 0)
4. → Create `/Users/ketema/GEMINI-ORCHESTRATED-AGENT.md`
5. → Create `.serena/memories/gemini-orchestration-configuration.md`
6. → Create `.serena/memories/orchestrator-best-practices.md`
7. → Test updates in next orchestration session
8. → Iterate based on results

---

## SUCCESS CRITERIA

**agent-coordination skill**:
- ✓ Step 0 orientation section added
- ✓ Orchestrator-as-human education included
- ✓ Tmux two-step protocol documented with examples
- ✓ Permission expectations clarified

**GEMINI-ORCHESTRATED-AGENT.md**:
- ✓ Full constitutional framework (CL1-CL9, QS1-QS6, M1-M5)
- ✓ Explicit orchestrator context ("you are orchestrated, not primary")
- ✓ CL5 override clearly stated
- ✓ Tmux protocol embedded
- ✓ Response template requirements
- ✓ Permission handling expectations

**Documentation**:
- ✓ Gemini configuration documented (flags, MCP servers)
- ✓ Orchestrator best practices captured
- ✓ Task assignment templates provided
- ✓ Troubleshooting guides included

---

**Plan Created**: 2025-11-18
**Orchestrator**: claude-orchestrator
**Source**: Euler Problems #29-32 orchestration learnings
**Next Action**: Submit plan to user for approval, then implement
