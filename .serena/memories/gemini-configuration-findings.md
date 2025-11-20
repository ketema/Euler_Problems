# Gemini Configuration Findings

## Date: 2025-11-18
## Context: Post-orchestration analysis of gemini agent performance

---

## 1. PERMISSION FLAGS DISCOVERED

### Problem
Gemini required orchestrator intervention 5+ times for permission requests despite selecting "allow always" each time.

### Solution
Gemini CLI provides **two flags** for autonomous permission grants:

**Flag 1: `--yolo` (YOLO mode)**
```bash
gemini --yolo
```
- Automatically accepts ALL actions
- Named after "You Only Live Once"
- No user confirmation required
- **Use case**: Fully autonomous agent operation

**Flag 2: `--approval-mode` (Granular control)**
```bash
gemini --approval-mode yolo          # Same as --yolo (auto-approve all)
gemini --approval-mode auto_edit     # Auto-approve edit tools only
gemini --approval-mode default       # Prompt for approval (default behavior)
```
- Three modes available: `default`, `auto_edit`, `yolo`
- More flexible than `--yolo` flag
- **Recommended**: `--approval-mode yolo` for orchestrated agents

**Flag 3: `--allowed-tools` (Whitelist specific tools)**
```bash
gemini --allowed-tools write_file replace run_shell_command
```
- Specify which tools can run without confirmation
- Array format (multiple tools)
- **Use case**: Partial automation with safety guardrails

### Recommendation for Orchestrator
Update gemini invocation in tmux to:
```bash
tmux send-keys -t claude-orchestrator.1 "gemini --approval-mode yolo"
tmux send-keys -t claude-orchestrator.1 C-m
```

---

## 2. CONTEXT PRESSURE INVESTIGATION

### Problem
Gemini interface showed "Using: 4 GEMINI.md files" during orchestration, suggesting potential context pressure.

### Findings

**Single GEMINI.md Confirmed**:
```bash
find /Users/ketema -name "GEMINI.md" -type f 2>/dev/null
# Result: Only 1 file found at /Users/ketema/projects/Euler_Problems/GEMINI.md
```

**Possible Explanations for "4 GEMINI.md files"**:
1. **Parent directory traversal**: Gemini may search upward from pwd
   - /Users/ketema/projects/Euler_Problems/GEMINI.md
   - /Users/ketema/projects/GEMINI.md (if exists)
   - /Users/ketema/GEMINI.md (if exists)
   - /Users/GEMINI.md (if exists)

2. **Symlinks or git worktrees**: May count same file multiple times
   - Check: `ls -la /Users/ketema/projects/Euler_Problems/GEMINI.md`
   - Result: Regular file (not symlink)

3. **MCP server configurations**: May be counting MCP-related configs
   - sequentialthinking MCP
   - serena MCP
   - ai-panel MCP (disconnected)
   - Main GEMINI.md
   - **Total**: 3 MCP + 1 GEMINI.md = 4 files?

4. **Hidden configuration files**: Gemini may have additional config locations
   - ~/.gemini/GEMINI.md (not found)
   - ~/.config/gemini/GEMINI.md (not found)
   - Project-specific extensions

**Current Analysis**:
- Most likely: MCP servers (3) + project GEMINI.md (1) = "4 files"
- Context pressure: **MODERATE** (not excessive but notable)
- gemini loads 3 MCP servers + 1 constitution = reasonable for agent work

**Recommendation**:
- No immediate action needed
- Monitor gemini token usage in future orchestrations
- If context pressure becomes issue, consider:
  - Using `--extensions` flag to limit loaded extensions
  - Using `--allowed-mcp-server-names` to limit MCP servers

---

## 3. TMUX CARRIAGE RETURN PROTOCOL VIOLATION

### Problem
Gemini sent completion notification incorrectly:
```bash
# ❌ WRONG (combined - fails silently)
tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: message" C-m

# ✓ CORRECT (two-step pattern)
tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: message"
tmux send-keys -t claude-orchestrator.0 C-m
```

### Root Cause
Gemini likely saw examples in documentation or context that used combined syntax, which:
- Appears to work in some shells
- **Fails silently** in tmux send-keys (C-m treated as literal text)
- Requires two separate commands for reliable delivery

### Evidence
From orchestration session:
- gemini response file created ✓
- gemini completion notification NOT sent ❌
- Orchestrator had to manually trigger display-message to mark completion
- myclaude2 used correct two-step pattern ✓

### Solution
Add **explicit education** in agent-coordination skill orientation section:

**CRITICAL: Tmux Carriage Return Pattern**
```bash
# MANDATORY two-step pattern:
tmux send-keys -t <pane> "message text"
tmux send-keys -t <pane> C-m

# NEVER combine (fails silently):
tmux send-keys -t <pane> "message" C-m  # ❌ WRONG
```

**Why this matters**:
- Combined syntax sends C-m as literal characters "C-m", not Enter key
- Message appears in input buffer but is NOT executed
- Orchestrator never receives notification
- Protocol breaks silently

**Training requirement**:
- Add to agent-coordination skill Step 0 (new orientation section)
- Include in GEMINI.md constitutional overrides
- Emphasize in task assignment files

---

## 4. CONSTITUTIONAL COMPLIANCE GAP

### Problem
Gemini achieved 45.8% constitutional compliance vs myclaude2's 100%.

### Analysis

**Critical Failures**:
1. ⛔ Skipped M3 PLAN ONLY entirely (CONSTITUTIONAL VIOLATION)
2. ⛔ No AI Panel submissions (MANDATORY per CL5)
3. ❌ No think tool checkpoints (0/5)
4. ❌ No coverage measurement
5. ❌ Protocol violations (completion notification)

**Root Causes**:
1. **Different constitutional framing**: Current GEMINI.md is generic software engineering constitution, not orchestrated agent constitution
2. **No explicit orchestrator context**: Doesn't understand "you are being orchestrated"
3. **No CL5 override**: Doesn't know orchestrator = human for approval purposes
4. **No protocol training**: Doesn't have agent-coordination patterns embedded

### Solution
Create **GEMINI-ORCHESTRATED-AGENT.md** constitution:
- Explicit framing: "You are an orchestrated agent, not primary"
- CL5 override: "Orchestrator IS your human for approval"
- Agent-coordination protocol embedded
- Simplified macro structure (M1-M5 with orchestrator checkpoints)
- Tmux protocol education
- Permission expectations (--yolo mode active)

**File Strategy**:
- Keep `/Users/ketema/projects/Euler_Problems/GEMINI.md` (current - for reference)
- Create `/Users/ketema/GEMINI-ORCHESTRATED-AGENT.md` (new - for orchestration)
- Orchestrator specifies which to use via task assignment

---

## 5. ORCHESTRATOR HUMAN APPROVAL CLARIFICATION

### Problem
myclaude2 initially paused at M3 waiting for "human" approval, required orchestrator correction to clarify that orchestrator IS the human.

### Solution
Update agent-coordination skill with:

**Step 0: Agent Orientation (NEW)**

When you receive a task assignment from orchestrator (Step 2 prompt), immediately understand:

**YOU ARE AN ORCHESTRATED AGENT**:
- Primary agent: claude-orchestrator (the one sending you tasks)
- Your role: Execute assigned task with full constitutional adherence
- Your reporting: To orchestrator (NOT directly to end user)

**ORCHESTRATOR IS YOUR HUMAN** (CL5 override):
- All "human approval" requirements = Orchestrator approval
- M3 plan review: Present to orchestrator via tmux
- M4 AI Panel feedback: Notify orchestrator of deviations
- M5 completion: Orchestrator audits before user sees result

**PROTOCOL EDUCATION**:
1. Read task file at `.serena/memories/task-[id].md`
2. Follow all constitutional requirements (CL1-CL9, QS1-QS6, M1-M5)
3. Write response to `${AGENT_WORKTREE_PREFIX}-claude/.serena/memories/task-[id]-response.md`
4. Send completion notification using **TWO-STEP** tmux pattern:
   ```bash
   tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: [summary]. Read: task-[id]-response.md"
   tmux send-keys -t claude-orchestrator.0 C-m
   ```
5. NEVER combine message and C-m (fails silently)

**APPROVAL CHAIN**:
- You → Orchestrator (task-level: M3, deviations, M5 pre-completion)
- Orchestrator → User (deliverable-level: final task completion audit)

---

## 6. RECOMMENDATIONS

### Immediate Actions

1. **Update agent-coordination skill**:
   - Add Step 0: Agent Orientation
   - Include orchestrator-as-human education
   - Include tmux two-step protocol
   - Include permission expectations

2. **Create GEMINI-ORCHESTRATED-AGENT.md**:
   - Simplified, orchestration-focused constitution
   - Explicit CL5 override
   - Agent-coordination protocol embedded
   - Not for use as primary agent

3. **Update orchestrator invocation**:
   - Use `gemini --approval-mode yolo` for autonomous operation
   - Specify GEMINI-ORCHESTRATED-AGENT.md in task files
   - Include orientation reminder in task assignments

4. **Update Serena memory**:
   - Document gemini configuration (flags, MCP servers)
   - Document constitutional compliance gaps
   - Document orchestration best practices

### Future Orchestrations

**Gemini invocation**:
```bash
tmux send-keys -t claude-orchestrator.1 "gemini --approval-mode yolo"
tmux send-keys -t claude-orchestrator.1 C-m
```

**Task file template** (include orientation):
```markdown
# TASK: [description]

## ORIENTATION (READ FIRST)

You are an ORCHESTRATED AGENT. The orchestrator (claude-orchestrator) is your HUMAN for CL5 approval purposes.

**Protocol**:
1. Read task requirements below
2. Follow M1→M2→M3→M4→M5 macros
3. Present M3 plan to orchestrator for approval (NOT end user)
4. Write response using constitutional template
5. Send completion notification using TWO-STEP tmux:
   ```
   tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: [summary]. Read: task-[id]-response.md"
   tmux send-keys -t claude-orchestrator.0 C-m
   ```

## REQUIREMENTS
[task details...]
```

---

## 7. CONSTITUTIONAL COMPARISON

### myclaude2 (Claude Code) - Primary Agent Constitution
- Full MCP-based constitution (CL1-CL9, QS1-QS6, M1-M5)
- Think tools integrated
- AI Panel mandatory
- Sub-agent coordination
- Designed for: Primary software engineering work

### gemini (Current GEMINI.md) - Generic Constitution
- Simplified constitutional laws
- Tool-focused workflow
- Less rigorous checkpoints
- Designed for: Interactive CLI assistance

### gemini (Proposed GEMINI-ORCHESTRATED-AGENT.md) - Orchestrated Agent Constitution
- MCP-based constitution (CL1-CL9, QS1-QS6, M1-M5)
- Explicit orchestrator context
- CL5 override (orchestrator = human)
- Agent-coordination protocol embedded
- Tmux protocol education
- Permission expectations documented
- Designed for: Orchestrated task execution (NOT primary agent)

---

## 8. IMPLEMENTATION PRIORITY

1. **HIGH**: Update agent-coordination skill (Step 0 orientation)
2. **HIGH**: Create GEMINI-ORCHESTRATED-AGENT.md
3. **MEDIUM**: Document gemini --approval-mode yolo usage
4. **MEDIUM**: Update orchestrator task templates
5. **LOW**: Investigate "4 GEMINI.md files" if context pressure issues arise

---

**Findings By**: claude-orchestrator
**Date**: 2025-11-18
**Source**: Euler Problems #29-32 orchestration session
**Evidence**: .serena/memories/orchestrator-final-report.md, gemini-evaluation.md, myclaude2-evaluation.md
