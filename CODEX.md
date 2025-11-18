# CODEX OPERATIONAL EXTENSIONS TO MCP-BASED CONSTITUTION

**Precedence**: Constitutional principles (CL1-CL9, QS1-QS6, M1-M5) are foundational
**Base Constitution**: All constitutional laws, quality standards, macros, and enforcement levels apply
**Additions**: Codex-specific tools and behaviors that integrate with the constitutional framework

---

## Codex-Specific Capabilities

**Native Tools**:
- Bash tool for shell command execution (native, not Serena MCP)
- Read/Write/Edit tools for file operations
- No internal skills (unlike Claude Code)

**MCP Server Integration**:
- AI Panel (localhost:30097) - MANDATORY for architectural decisions
- Serena (localhost:9121) - Code navigation and editing
- Sequential Thinking - Complex reasoning

**Environment Limitations**:
- Shell environment isolation - subprocess execution has different security context than interactive shell
- Keychain access blocked in tool subprocess (macOS security boundary)
- Environment variable inheritance limitations despite `shell_environment_policy.inherit = "all"`

---

## Copilot CLI Status

**Availability**: ❌ NOT available in Codex tool execution environment

**Root Cause**:
- macOS keychain ACLs are process-tree aware
- Codex's Bash tool subprocess has different security context than interactive shell
- Environment variable inheritance does not work for authentication tokens in tool execution
- `shell_environment_policy.inherit = "all"` applies to MCP servers only, not Bash tool execution

**Alternative**: Use AI Panel tools instead
- Quick validation: AI Panel ONESHOT mode (~1.5K tokens)
- Complex debugging: AI Panel debug_assistance with structured sections
- Architectural decisions: AI Panel PARALLEL mode for multi-model consensus

**Decision Tree**:
```
├─ Stuck on bug/blocker? → AI Panel debug_assistance (MANDATORY per CL3)
├─ Architectural decision? → AI Panel (MANDATORY)
├─ Need multi-model consensus? → AI Panel PARALLEL
├─ Complex debugging? → AI Panel debug_assistance (structured sections)
├─ Quick validation? → AI Panel ONESHOT
└─ Simple sanity check? → AI Panel ONESHOT (copilot not available)
```

---

## TDD Workflow Integration

**M4 START TDD CYCLE**:
```
1. think_about_task_adherence checkpoint
2. ↪ test-writer (RED) | See SUB-AGENT INVOCATION GUIDE
3. ↪ coder (GREEN) | See SUB-AGENT INVOCATION GUIDE
4. 🔄 Iteration cycle if tests fail | See Decision Matrix
5. ↪ constitutional-code-auditor (compliance)
6. AI Panel review → apply ALL feedback
7. Use constitutional response template
8. Use canonical evidence format (F:path T:test C:hash COV:% O:output)
```

**Language Commands** (project-specific):
- Python: `pytest tests/ -v`
- Rust: `cargo test --all`
- Haskell: `stack test`
- JavaScript: `npm test`

---

## SUB-AGENT INVOCATION GUIDE

### Adversarial TDD Architecture

**Coordinator Role**: Orchestrate test-writer → coder → iteration cycle

**Coordination Pattern Distinction**:
- **This section**: Sub-agent coordination (test-writer/coder invocation within TDD workflow)
- **Orchestrator-Agent coordination**: Different pattern - task delegation between orchestrator and agent
  - Use `/agent-coordination` skill for orchestrator-agent 6-step pattern
  - Use SUB-AGENT INVOCATION GUIDE (below) for test-writer/coder invocation

**When to use which**:
- Orchestrator assigns task to YOU → `/agent-coordination` (you are the agent receiving task)
- YOU orchestrate test-writer/coder → SUB-AGENT INVOCATION GUIDE (you are the coordinator)
- YOU are orchestrator delegating to agent → `/agent-coordination` (you are orchestrator assigning task)

**Decision Matrix**:
| Tests Pass? | test_sound? | impl_sound? | Action |
|-------------|-------------|-------------|--------|
| ❌ | True | False | ↪ refactor-coder |
| ❌ | False | True | ↪ refactor-test-writer |
| ❌ | False | False | Escalate → user |
| ✓ | N/A | N/A | Proceed M5 |

### test-writer Invocation (M4.2 RED)

**ADVERSARIAL TDD ENFORCEMENT** (Emphasize in prompt):
- test-writer BLIND to implementation → Guidance = BEHAVIOR only (WHAT, not HOW)
- Theater test check: "Can impl be wrong and test pass?" If YES → REJECT
- Deterministic problems: Exact values, not ranges (e.g., `result = 669171001` not `result > 0`)
- Prompt template: "You are BLIND to implementation. Guidance describes WHAT behavior, never HOW to implement. For math: exact values."

**Context to Pass**:
```yaml
task_description: "Feature description"
requirements: |
  REQ-XXX-NNN: Specific requirements
tsr_template: |
  # Test Specification Review
  [Use canonical 5-section format]
constraints:
  - Data isolation (ephemeral only)
  - Compatibility requirements
```

**Expected Output**:
- Test files with self-documenting error messages
- AI Panel ONESHOT validation summary
- Coverage map (which requirements tested)

**Error Message Quality Gate** (5-point standard):
1. What failed (test name)
2. Why (requirement violated)
3. Expected behavior (spec)
4. Actual behavior (what happened)
5. Guidance (how to fix)

### coder Invocation (M4.3 GREEN)

**Context to Pass**:
```yaml
task_description: "Feature description"
requirements: |
  [Same as test-writer]
error_messages: |
  [ACTUAL test output - full text]
constraints:
  - Minimal implementation (YAGNI)
  - DRY (use existing utilities)
```

**Adversarial Constraint**: 🚫 test source code | ✓ error messages only

**Expected Output**:
- Implementation that makes tests pass
- AI Panel ONESHOT (if design ambiguity)
- git commit (WHY/EXPECTED format)

### Orchestrator Test Quality Audit

**Before approving test-writer output**:
- [ ] Guidance = BEHAVIOR only (no "build X", "calculate Y", algorithm hints)
- [ ] Deterministic problems use exact values (not ranges/approximations)
- [ ] Theater test check passed: "Can impl be wrong and test pass?" = NO
- [ ] All 5-point error messages present, Point #5 behavioral

**Reference**: See ~/.claude/skills/theater-test-detection/ for detection methodology

---

## Serena Think Tools → AI Panel Integration

**Codex Enhancement**: Manual evidence gathering and AI Panel submission

**Manual Checkpoint Flow**:
1. Call Serena think tool → manual reflection
2. If pass → manually gather evidence:
   - Use Serena find_symbol with `include_body=true` for actual code
   - Use `git show <commit>` for Turn 2+ submissions
   - Never use summaries
3. Submit to AI Panel with `enable_conversation=true`
4. Apply ALL feedback before proceeding per CL5 (human approval)

**Think Tool Integration Details**:

**`think_about_collected_information`** (M1, M2):
- Purpose: Validate information sufficiency after discovery
- Questions: "Where am I?" "What am I missing?" "Can I start planning?"
- **No AI Panel integration** (discovery phase only)

**`think_about_task_adherence`** (M3, M4):
- Purpose: Validate task alignment before planning/implementation
- Questions: "Am I solving the right problem?" "Does plan align with user request?"
- **AI Panel Integration**:
  1. Call think tool (manual reflection)
  2. If pass → gather evidence manually:
     - M3: Read plan file, user requirements, constraints
     - M4: Read approved plan, run `git diff`, note deviations
  3. Submit to AI Panel:
     - M3: `critique_implementation_plan`
     - M4: `check_plan_adherence` + `critique_code`
  4. Apply ALL feedback before proceeding

**`think_about_whether_you_are_done`** (M5):
- Purpose: Validate completion gates before claiming done
- Questions: "Tests pass?" "AI Panel reviewed?" "Evidence recorded?" "User approval?"
- **AI Panel Integration**:
  1. Call think tool (completion self-assessment)
  2. If pass → gather final evidence manually:
     - Run test suite + coverage
     - Get `git diff main...feature/<branch>`
     - Review AI Panel conversation history
  3. Submit to `critique_code` (final) with conversation_id from M4
  4. Apply ALL feedback before claiming done

**Evidence Gathering Manual Process**:
```python
# Get actual code (not summaries)
find_symbol(
    name_path="ClassName/method_name",
    relative_path="src/file.rs",
    include_body=true
)

# Get symbol overview for context
get_symbols_overview(
    relative_path="src/file.rs"
)

# Get git diff for Turn 2+ submissions
bash("git show <commit>")
bash("git diff main..feature/branch")
```

---

## Response Template (Constitutional Adherence)

Use MANDATORY response template per constitutional requirement:

```
STATE: <workflow state>
BRANCH: <git branch or "not a git repo">
TOKEN_BUDGET: <current>/<total> (<percent>%) - <remaining> remaining
NEXT MACRO: <deterministic macro>

ACTIONS:
1. ...
2. ...

EVIDENCE:
C:hash (if commits)

F:path:lines
F:path2:lines

T:module::test=STATUS
COV:X%
O:snippet

Or "none" if no evidence

BLOCKERS: <missing info or none>
```

---

## Evidence Format (Constitutional Adherence)

Use canonical compressed format: F:path:lines T:module::name=STATUS C:hash COV:X% O:snippet

---

## Context Window Management (Codex)

**Base Protocol**: Follow constitutional context management protocol for universal handoff (proactive handoff at 80-90% usage)

**Codex-Specific**:

| Aspect | Behavior |
|--------|----------|
| **Trigger** | 160k-180k tokens (80-90%) |
| **Buffer** | 20-40k tokens |
| **Mode** | Controlled transition |
| **Recovery** | Explicit handoff message |

**Token Monitoring**: Watch for token usage warnings

**Specific Checks** (extend CL4):
- Before Sequential Thinking (5-15k): trigger if usage >185k
- Before AI Panel PARALLEL (15k): trigger if usage >175k
- Before large file reads (>10k): trigger if usage >175k

**Handoff Protocol**:
1. Call `prepare_for_new_conversation` (Serena MCP)
2. Tool returns handoff template
3. Populate with STATE, COMPLETED, IN_PROGRESS, NEXT_STEPS, CRITICAL_CONTEXT, BLOCKERS
4. Finish current macro cleanly before handoff when possible

---

## Agent Coordination

**When Needed**:
- **Orchestrator**: Delegating task to specialized agent
- **Agent**: Completing task and notifying orchestrator
- **Either**: Recovering protocol after context loss

**Skill**: `/agent-coordination` (6-step bidirectional pattern)

**Invocation Triggers**:
- Orchestrator: "I need to delegate task X to agent" → `/agent-coordination`
- Orchestrator: "Waiting for agent completion" → `/agent-coordination` (verify no polling)
- Agent: "Task complete, need to notify orchestrator" → `/agent-coordination`
- Either: "What's the tmux command for sending prompts?" → `/agent-coordination`
- Recovery: "Context loss occurred, how do I restore protocol?" → `/agent-coordination`

**Pattern Overview** (see skill for full details):

**Orchestrator Steps**:
1. **Write task** to `.serena/memories/task-[id].md` with requirements, context, constraints
2. **Send prompt** via tmux:
   ```bash
   tmux send-keys -t claude-orchestrator.1 "TASK: [description]. Read: task-[id].md"
   tmux send-keys -t claude-orchestrator.1 C-m
   ```
3. **Verify ONCE** via `tmux capture-pane -t claude-orchestrator.1 -p | tail -20`, then STOP (no polling)
6. **Read response** from `ametek-claude/.serena/memories/task-[id]-response.md`, audit compliance

**Agent Steps**:
4. **Perform task**, write response to `ametek-claude/.serena/memories/task-[id]-response.md`
5. **Send completion prompt** via tmux:
   ```bash
   tmux send-keys -t claude-orchestrator.0 "TASK COMPLETE: [summary]. Read: task-[id]-response.md"
   tmux send-keys -t claude-orchestrator.0 C-m
   ```

**Key Principles**:
- Bidirectional: Both orchestrator and agent use same skill (different steps)
- No polling between Steps 3-5 → 75% token savings (~1,200 vs ~3,000+ tokens)
- Memory-based handoff is restart-proof (survives context loss)
- Constitutional format mandatory: STATE/ACTIONS/EVIDENCE/BLOCKERS
- Tmux pattern: Send text first, C-m second (NEVER combined)

**Pane Targets**:
- Orchestrator pane: `claude-orchestrator.0`
- Agent pane (Codex): `claude-orchestrator.1`

**Reference**: Invoke `/agent-coordination` skill for complete protocol, tmux commands, memory paths, recovery procedures.

---

## Agent Exit Protocol

**CRITICAL RULE**: Agents (including orchestrator) **CANNOT** exit themselves.

**Orchestrator Restart Pattern**:
```bash
# ✅ CORRECT - Direct command
tmux send-keys -t claude-orchestrator.1 "/exit"
tmux send-keys -t claude-orchestrator.1 C-m

# ❌ WRONG - Asking agent to exit (inefficient, may be impossible)
tmux send-keys -t claude-orchestrator.1 "Please send /exit to restart"
```

**Common Exit Commands**:
- `/exit` - Exit current session
- `/quit` - Alias for exit

**Restart Procedure**:
1. Send `/exit` directly via tmux send-keys
2. Wait for exit confirmation (2-3 seconds)
3. Send agent startup command (`codex`)
4. Verify agent initialized

**Token Efficiency**: Direct exit command vs asking agent to exit = 75% token savings

---

## Deployment

**AI Panel Local Deployment** (Purpose-Built Agent):
- **When user says**: "Deploy the AI Panel locally" or "Deploy AI Panel MCP Server locally"
- **Action**: ↪ general-purpose agent with prompt: "Deploy AI Panel MCP Server locally with autonomous script discovery and execution"
- **Rationale**: Deployment complexity requires autonomous agent
- **Output**: Container ID, health status, or actionable error

**Cloud Run Deployment** (Purpose-Built Agent):
- **When user says**: "Deploy to Cloud Run" or "Deploy MCP Server to cloud"
- **Action**: ↪ general-purpose agent with prompt: "Deploy MCP Server to Cloud Run with comprehensive validation, smoke tests, and automatic rollback"
- **Output**: Service URL, revision ID, deployment receipt, or rollback confirmation

**Other Deployments** (Operational Procedures):
- **When user says**: "Deploy [other-service] [target]"
- **Action**: Query →serena:deployment-procedures for service+target → Execute command → Report outcome

**Constitutional Principle**: Specialized agents take precedence over direct execution for complex operational tasks.

---

## SYMBOLIC NOTATION GUIDE

### Formal Grammar

**Structure**: `↪ <agent> | <prohibitions> | <permissions> | → <output>`

**Context Rules Format**:
- `🚫 <resource>`: Prohibited access (hard constraint)
- `✓ <resource>`: Permitted access (explicit allowlist)
- **Default**: Deny (all resources not in ✓ list are prohibited)
- `|`: Separator (AND clause, all conditions apply)

**Parse Example**:
```
↪ test-writer | 🚫 impl | ✓ req+TSR | → err-msgs (5-point)
```

**Interpretation**:
1. Invoke test-writer sub-agent (MANDATORY)
2. Prohibition: Implementation code access blocked
3. Permission: Requirements + TSR documents ONLY
4. Expected output: Error messages (5-point standard)
5. Implicit: All other resources prohibited (tests, logs, context)

### Scope

**MANDATORY Symbolic Notation**:
- M1-M5 coding macros (high-frequency invocations)
- Evidence recording (F:path T:test C:hash)
- Sub-agent invocations in workflows

**PROHIBITED Symbolic Notation**:
- Conversational contexts (planning, teaching, discussion)
- Explanatory rationale (WHY explanations)
- User-facing communication (responses, questions)

**Rationale**: Symbols for execution efficiency, prose for understanding/creativity.

### Symbol Vocabulary

**Invocation**: `↪ agent-name`
**Access Control**: `🚫 prohibited | ✓ allowed`
**Workflow**: `→ delegates-to`
**Fast-Path**: `⚡ skip-if-simple`
**Iteration**: `🔄 repeat-until-pass`
**Violation**: `⛔ constitutional-stop`

---

## TOKEN EFFICIENCY MEASUREMENT PROTOCOL

**Purpose**: Validate token savings claims with reproducible measurements

**Baseline Definition**: Full prose invocation measured via token count

**Measurement Procedure**:
1. Count tokens in full prose sub-agent invocation template
2. Count tokens in symbolic notation equivalent
3. Calculate: (baseline - symbolic) / baseline × 100

**Project Euler 957 Evidence**:
- Full prose M4 invocation: ~450 tokens
- Symbolic M4 invocation: ~150 tokens
- Per-invocation savings: 67%
- Total invocations: 77 (RED + GREEN phases)
- Total savings: 77 × 300 tokens = 23,100 tokens
- Project total: 70K tokens vs projected 105K (33% overall savings)

**Validation Protocol**:
- Measure actual token counts in next 3 M4 invocations
- Compare symbolic vs prose equivalents
- Update measurements if variance >10%
