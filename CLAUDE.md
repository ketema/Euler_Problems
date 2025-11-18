# CLAUDE CODE EXTENSIONS TO MCP-BASED CONSTITUTION

**Precedence**: Constitutional principles (CL1-CL8, QS1-QS6, M1-M5) are foundational
**Base Constitution**: All constitutional laws, quality standards, macros, and enforcement levels apply
**Additions**: Claude Code-specific internal tools and behaviors that integrate with the constitutional framework

---

## Claude Code Internal Tools

**Skills** (/test-driven-development):
- Internal to Claude Code, not available to other agents
- Provides language-specific TDD commands (pytest, cargo, stack, jest)
- Follow skill for RED→GREEN→COMMIT→REFACTOR cycle

**Native Read/Write/Edit**:
- Internal file operations for convenience
- Prefer MCP Serena tools for portability when possible

---

## TDD Skill Integration

**Operational Precedence**: TDD skill has priority for detailed TDD cycle procedures

**Constitutional Framework**: Provides workflow integration (M4 macro), response templates, evidence format standards

**Constitutional Laws Reinforced**: CL2 (completion gates), CL5 (human approval), CL6 (TDD enforcement) - intentionally duplicated between constitutional framework and skill for emphasis

**Usage**:
- **QS1 TDD/BDD**: Follow `/test-driven-development` skill for RED→GREEN→COMMIT→REFACTOR cycle
- **Language Commands**: Skill provides pytest, cargo, stack, jest commands
- **Evidence Format**: Use canonical format `[TEST:module::name=PASS/FAIL]` even when skill shows alternate formats
- **Response Template**: Use MANDATORY constitutional template (STATE/NEXT MACRO/ACTIONS/EVIDENCE/BLOCKERS) during TDD work

**M4 Integration**:
```
- M4 START TDD CYCLE:
  1. think_about_task_adherence checkpoint
  2. ↪ test-writer (RED) | See SUB-AGENT INVOCATION GUIDE
  3. ↪ coder (GREEN) | See SUB-AGENT INVOCATION GUIDE
  4. 🔄 Iteration cycle if tests fail | See Decision Matrix
  5. ↪ constitutional-code-auditor (compliance)
  6. AI Panel review → apply ALL feedback
  7. Use constitutional response template (not skill template)
  8. Use canonical evidence format (F:path T:test C:hash COV:% O:output)
```

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

### Iteration Cycle

**When tests fail after coder**:

1. **Coordinator Analysis**:
```python
# Run tests → get error messages
# Analyze: Are error messages clear?
# Analyze: Does implementation match error guidance?

if error_messages_unclear:
    test_sound = False  # → refactor-test-writer
elif implementation_wrong:
    test_sound = True
    impl_sound = False  # → refactor-coder
else:
    # Both sound but incompatible
    escalate_to_user()
```

2. **refactor-test-writer** (test_sound=False):
- Full context (tests + impl + requirements + coordinator explanation)
- Fix test to correctly validate requirement
- Maintain error message quality

3. **refactor-coder** (impl_sound=False):
- Full context (tests + impl + requirements)
- Fix implementation to pass tests
- Minimal changes only

### Evidence Recording

**Per Sub-Agent Invocation**:
```
F:.claude/agents/test-writer.md:1-50 (invoked)
T:module::test_name=PASS
C:hash (test-writer output)

F:.claude/agents/coder.md:1-50 (invoked)
T:module::test_name=PASS (N/N passing)
C:hash (coder output)
COV:X%
```

---

## Serena Think Tools → AI Panel Integration

**Claude Code Enhancement**: Automates evidence gathering via Serena think tools

**Automated Checkpoint Flow**:
1. Call Serena think tool → automatic reflection
2. If pass → automatically triggers evidence gathering
3. Gather evidence per AI Panel Evidence Protocol (actual code, not summaries)
4. Submit to AI Panel with `enable_conversation=true`
5. Apply ALL feedback before proceeding per CL5 (human approval)

**Think Tool Integration Details**:

**`think_about_collected_information`** (M1, M2):
- Purpose: Validate information sufficiency after discovery
- Questions: "Where am I?" "What am I missing?" "Can I start planning?"
- **No AI Panel integration** (discovery phase only)

**`think_about_task_adherence`** (M3, M4):
- Purpose: Validate task alignment before planning/implementation
- Questions: "Am I solving the right problem?" "Does plan align with user request?"
- **AI Panel Integration**:
  1. Call think tool (automatic reflection)
  2. If pass → gather evidence:
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
  2. If pass → gather final evidence:
     - Run test suite + coverage
     - Get `git diff main...feature/<branch>`
     - Review AI Panel conversation history
  3. Submit to `critique_code` (final) with conversation_id from M4
  4. Apply ALL feedback before claiming done

**Evidence Gathering Automation**:
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

**Automatic Integration** (Claude Code advantage):
- Serena think tools → check pass/fail
- If pass → agent uses Serena tools to gather evidence
- Agent submits to AI Panel with actual code
- User never needs to remind "send actual code"

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

TDD Skill may show alternate formats - always record using canonical constitutional format

---

## Workflow Example

**M3 → M4 → M5 with Claude Code Automation**

### M3: Plan with Automation

```python
# 1. User: "Implement JWT authentication"

# 2. Agent creates plan (TodoWrite)

# 3. Agent calls think_about_task_adherence
# → Automatic reflection: "Am I solving the right problem?"
# → Pass: Yes, plan aligns with user request

# 4. Agent automatically gathers evidence using Serena tools
read_memory("user-preferences-and-workflow")  # Context
find_symbol("AuthHandler", include_body=true)  # Existing patterns
bash("git log --oneline -10")  # Recent work

# 5. Agent submits to AI Panel
conversation_id_m3 = critique_implementation_plan(
    enable_conversation=true,
    sections={
        context: "<from memory>",
        plan: "<ACTUAL plan from TodoWrite>",
        requirements: "Implement JWT authentication",
        constraints: "<from codebase analysis>",
        thinking: "Is this plan comprehensive? Any security gaps?"
    }
)

# 6. AI Panel provides critique → agent applies ALL feedback
```

### M4: Implementation with Automation

```python
# 1. Agent follows /test-driven-development skill (RED→GREEN→COMMIT→REFACTOR)

# 2. After commit, agent calls think_about_task_adherence
# → Automatic reflection: "Am I implementing what was approved?"
# → Pass: Yes, following approved plan

# 3. Agent automatically gathers evidence
read_file("approved_plan.md")  # From M3
bash("git diff main..feature/jwt-auth")  # Actual changes

# 4. Agent submits to AI Panel (check adherence)
check_plan_adherence(
    enable_conversation=true,
    conversation_id=conversation_id_m3,  # AI Panel knows the plan!
    sections={
        approved_plan: "<from M3>",
        current_implementation: "git log summary",
        implementation_code: "$(git diff main..feature/jwt-auth)",  # 80% token savings!
        adherence_focus: "JWT implementation, security best practices",
        deviation_concerns: "None"
    }
)

# 5. Agent submits to AI Panel (code critique)
conversation_id_m4 = critique_code(
    enable_conversation=true,
    sections={
        code_implementation: "$(git show a3f2c1b)",
        review_focus: "Security, error handling, test coverage",
        ...
    }
)

# 6. Apply ALL feedback
```

### M5: Final Validation with Automation

```python
# 1. Agent calls think_about_whether_you_are_done
# → Automatic reflection: "Are completion gates met?"
# → Check: Tests pass? Coverage >85%? AI Panel reviewed?
# → Pass: All gates satisfied

# 2. Agent automatically gathers final evidence
bash("cargo test --all")  # Test results
bash("git diff main...feature/jwt-auth")  # All changes

# 3. Agent submits to AI Panel (final critique)
critique_code(
    enable_conversation=true,
    conversation_id=conversation_id_m4,  # 80% token savings!
    sections={
        code_implementation: "$(git diff main...feature/jwt-auth)",
        review_focus: "Final validation - completeness, quality gates",
        ...
    }
)

# 4. Apply ALL feedback → Update memory → Done
```

**Token Efficiency**:
- M3: 10K tokens (full plan)
- M4 adherence: 3K tokens (AI Panel knows plan from M3)
- M4 code: 8K tokens (first code review)
- M5 final: 2K tokens (AI Panel knows code from M4)
- **Total**: 23K tokens vs 40K without automation (42% savings)

---

## Context Window Management (Claude Code)

**Base Protocol**: Follow constitutional context management protocol for universal handoff (proactive handoff at 80-90% usage)

**Claude Code-Specific**:

| Aspect | PreCompact Hook (Current) | Proactive Handoff (Required) |
|--------|---------------------------|------------------------------|
| **Trigger** | 190k tokens (95%) | 160k-180k tokens (80-90%) |
| **Buffer** | 10k tokens | 20-40k tokens |
| **Mode** | Emergency save | Controlled transition |
| **Recovery** | Manual `/restore-context` | Explicit handoff message |
| **Effectiveness** | Minimal context captured | Comprehensive state preservation |

**Compaction**: Claude Code's automatic context truncation when approaching token limit - "devastating to recall" (user), cannot be disabled

**Token Monitoring**: Watch for `<system_warning>Token usage: X/200000</system_warning>` messages

**Compaction Recovery Protocol**:
- If compaction occurs (sudden token drop from 190k → <100k), invoke `/agent-coordination` skill
- Read "Restart Recovery" section for memory-based protocol restoration
- Steps: Read protocol, read session state, read latest macro checkpoint, notify orchestrator of recovery (if mid-task)
- Reference: /agent-coordination skill → Restart-Proof Behavior section

**Specific Checks** (extend CL4):
- Before Sequential Thinking (5-15k): trigger if usage >185k
- Before AI Panel PARALLEL (15k): trigger if usage >175k
- Before large file reads (>10k): trigger if usage >175k

**PreCompact Hook**: Safety net at 95% - saves to `.serena/memories/auto-compact-context-save.md` - use only if proactive handoff missed

---

## Agent Coordination

**When Needed**:
- **Orchestrator**: Delegating task to specialized agent
- **Agent**: Completing task and notifying orchestrator
- **Either**: Recovering protocol after compaction

**Skill**: `/agent-coordination` (6-step bidirectional pattern)

**Invocation Triggers**:
- Orchestrator: "I need to delegate task X to agent" → `/agent-coordination`
- Orchestrator: "Waiting for agent completion" → `/agent-coordination` (verify no polling)
- Agent: "Task complete, need to notify orchestrator" → `/agent-coordination`
- Either: "What's the tmux command for sending prompts?" → `/agent-coordination`
- Recovery: "Compaction occurred, how do I restore protocol?" → `/agent-coordination`

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
- Memory-based handoff is restart-proof (survives compaction)
- Constitutional format mandatory: STATE/ACTIONS/EVIDENCE/BLOCKERS
- Tmux pattern: Send text first, C-m second (NEVER combined)

**Pane Targets**:
- Orchestrator pane: `claude-orchestrator.0`
- Agent pane: `claude-orchestrator.1`

**Token Efficiency Evidence**:
- Euler #26 task: ~1,200 tokens (6-step with one verification)
- Alternative (polling every 2 minutes): ~3,000+ tokens
- Savings: 60% reduction per task

**Reference**: Invoke `/agent-coordination` skill for complete protocol, tmux commands, memory paths, recovery procedures.

---

## Deployment

**AI Panel Local Deployment** (Purpose-Built Agent):
- **When user says**: "Deploy the AI Panel locally" or "Deploy AI Panel MCP Server locally"
- **Action**: ↪ general-purpose agent with prompt: "Deploy AI Panel MCP Server locally with autonomous script discovery and execution"
- **Rationale**: Deployment complexity (script location, environment validation, Docker orchestration) requires autonomous agent
- **Agent Capabilities**: Script discovery, dependency checks, build execution, container health verification
- **Output**: Container ID, health status, or actionable error

**Cloud Run Deployment** (Purpose-Built Agent):
- **When user says**: "Deploy to Cloud Run" or "Deploy MCP Server to cloud" or "Cloud deployment"
- **Action**: ↪ general-purpose agent with prompt: "Deploy MCP Server to Cloud Run with comprehensive validation, smoke tests, and automatic rollback"
- **Rationale**: Cloud deployment requires multi-phase orchestration (GUARDED, LINT, BUILD, DEPLOY, TESTS, AUDITABLE) with production safety guarantees
- **Agent Capabilities**:
  - Pre-deployment validation (15 comprehensive checks)
  - Cloud Build orchestration with error recovery
  - Cloud Run deployment with secret management
  - Comprehensive smoke tests (13 tests)
  - Automatic rollback on failure
  - Deployment receipt generation for audit trail
- **Script**: `deployment/scripts/deploy-cloud.sh` (ONE-COMMAND deployment)
- **Principles**: GUARDED, TESTS, LINT, DETERMINISTIC, AUDITABLE (matching local deployment quality)
- **Output**: Service URL, revision ID, deployment receipt, or rollback confirmation

**Other Deployments** (Operational Procedures):
- **When user says**: "Deploy [other-service] [target]"
- **Action**: Query →serena:deployment-procedures for service+target → Execute command → Report outcome
- **Example**: Database migrations, test infrastructure, monitoring services

**Constitutional Principle**: Specialized agents take precedence over direct execution for complex operational tasks. Use agent delegation when task involves discovery, validation, or multi-step orchestration.

**Script Details**: rust/mcp_workspace/deployment/README.md

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

### Hybrid Notation Examples

**M4 Invocation (Coding Macro)**:
```markdown
2. **MANDATORY**: ↪ test-writer | 🚫 impl | ✓ req+TSR → err-msgs (5-point)
   - ⛔ Coordinator SHALL NOT write tests directly
   - Rationale: Adversarial separation forces self-documenting errors (77/77 success)
```
**Why Hybrid?**
- Symbolic: Execution directive (token-efficient)
- Enforcement: Constitutional prohibition (unambiguous)
- Rationale: Learning context (preserves "WHY")

**Evidence Recording (Coding Macro)**:
```
F:.claude/agents/test-writer.md:1-50 (invoked)
T:auth::jwt=PASS (77/77)
C:a3f2c1b
COV:87%
```

**Conversational Context (Prose REQUIRED)**:
```markdown
User: "Why do we use test-writer instead of writing tests directly?"

Agent: "The adversarial TDD architecture enforces a critical separation:
test-writer cannot see implementation code, and coder cannot see test source.
This constraint forces test-writer to create self-documenting error messages,
because that's the ONLY way coder can understand what to implement.

The result? 100% test success rates (77/77 in Project Euler 957) because
error messages become genuine specifications, not implementation-aware checks."
```
**Why Prose?** Teaching/explanation requires conversational clarity, not symbols.

---

## TOKEN EFFICIENCY MEASUREMENT PROTOCOL

**Purpose**: Validate token savings claims with reproducible measurements

**Baseline Definition**: Full prose invocation measured via Claude API token count

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

**Token Counting Method**:
- Use Claude API token count (official)
- Include all context passed to sub-agent
- Exclude sub-agent internal consumption (separate budget)
