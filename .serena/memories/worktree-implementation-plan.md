# Worktree Management Implementation Plan for Euler_Problems

**Created**: 2025-01-18
**Project**: Euler_Problems
**Reference**: ametek_chess worktree_manager.sh (working baseline)

## Project Comparison Analysis

### Similarities (Core Infrastructure)
1. **Multi-agent coordination needs**
   - Both projects use orchestrator + agents pattern
   - tmux-based agent communication
   - Persistent memory for task coordination (.serena/memories/)
   - Git worktrees for agent isolation

2. **Build isolation requirements**
   - Agent-specific target directories prevent artifact conflicts
   - Language-specific build systems (Rust: cargo, Haskell: stack, etc.)
   - Concurrent agent execution without interference

3. **Environment isolation**
   - Agent-specific .envrc files
   - Deterministic port assignment (hash-based)
   - Separate logs/temp directories per agent

### Differences (Critical for Implementation)

| Aspect | ametek_chess | Euler_Problems |
|--------|--------------|----------------|
| **Database** | PostgreSQL required (ketema_chess_ephemeral_<agent>) | ❌ None needed |
| **Worktree Prefix** | `ametek-<agent>` | `euler-<agent>` |
| **Project Structure** | Monorepo (python/, rust/, haskell/, database/, monitoring/) | Problem-based (problem1/, problem11/, problem28/) |
| **Build Targets** | Unified Cargo workspace | Per-problem builds (problem11/haskell/, problem13/rust/) |
| **Test Database** | Template DB (ketema_chess_dev_ephemeral) | N/A |
| **Schema Migrations** | Auto-applied via SQLx/Alembic | N/A |
| **Dependency Sync** | Cross-language (Poetry, Cargo, Stack) | Per-problem isolation |

### Configuration Extracted

**ametek_chess** (line references to worktree_manager.sh):
- Worktree pattern: `ametek-$agent_name` (line 83, 122, 377+)
- Database naming: `ketema_chess_ephemeral_${agent_name}` (line 684)
- Template DB: `ketema_chess_dev_ephemeral` (line 685)
- PostgreSQL config: POSTGRES_HOST, POSTGRES_PORT, POSTGRES_USER (line 686-688)

**Euler_Problems** (required):
- Worktree pattern: `euler-$agent_name`
- Database naming: N/A (skip all DB functions)
- Build isolation: `target-$agent_name` (per-problem Rust/Haskell builds)
- Port range: ❌ NOT NEEDED (no web servers, no listening services - YAGNI)

## Implementation Plan

### Phase 1: Script Structure (YAGNI-Compliant)

**File**: `scripts/worktree_manager.sh` (Euler_Problems project root)

**Approach**: Copy + Adapt (not abstract)
- Start from ametek_chess/scripts/worktree_manager.sh
- Remove database functions entirely (create_agent_database, remove_agent_database, list_agent_databases, cleanup_orphan_databases)
- Change hardcoded "ametek" → "euler"
- Remove port generation logic (generate_agent_port) - NOT NEEDED for Euler
- Remove AGENT_PORT from .envrc template - no listening services
- Keep: create, remove, list, sync, health-check, envrc management

**Justification**:
- Two projects don't justify abstraction overhead (YAGNI)
- Copy allows per-project customization
- If 3rd project emerges → then abstract common patterns

### Phase 2: Project-Specific Customization

**Configuration Variables** (top of script):
```bash
# Project-specific configuration
readonly PROJECT_NAME="euler"
readonly WORKTREE_PREFIX="euler"
readonly REQUIRES_DATABASE=false
readonly REQUIRES_PORTS=false  # No web servers or listening services
```

**Functions to Keep** (from ametek_chess):
1. `list_agents` - List worktrees
2. `create_agent` - Create worktree + environment
3. `setup_agent_environment` - Logs, temp, .vscode
4. `generate_envrc` - Agent-specific .envrc
5. `sync_envrc` - Sync templates
6. `init_templates` - Template initialization
7. `validate_template` - Security checks
8. `backup_envrc` / `restore_envrc` - Safety
9. `update_state` - Timestamp tracking
10. `health_check` - Environment validation
11. `remove_agent` - Cleanup worktree
12. `cleanup_worktrees` - Prune orphans
13. `worktree_status` - Health reporting

**Functions to Remove**:
- `create_agent_database` (line 682-714)
- `remove_agent_database` (line 717-746)
- `list_agent_databases` (line 749-760)
- `cleanup_orphan_databases` (line 763-816)
- `sync_dependencies` (line 819-848) - Euler problems isolated, no global sync
- `generate_agent_port` (line 245-254) - No listening services in Euler_Problems

**Modified Functions**:

1. `setup_agent_environment` (line 120-188):
   - Remove lines 149-158 (port generation and validation)
   - Keep: logs/, .tmp-<agent>/, .vscode/ setup

2. `create_agent` (line 69-117):
   - Remove lines 111-114 (database creation call)
   - Keep environment setup, .envrc, .vscode

3. `remove_agent` (line 631-677):
   - Remove line 674 (database cleanup call)
   - Keep worktree removal, safety checks

4. `generate_envrc` (line 257-357):
   - Remove lines 306-311 (port generation call)
   - Remove PORT placeholder from template substitution (line 324)
   - **SECURITY**: Add input validation before substitution
     ```bash
     # Validate agent_name (prevent injection)
     if [[ ! "$agent_name" =~ ^[a-zA-Z0-9_-]+$ ]]; then
         log_error "Invalid agent name format: $agent_name"
         return 1
     fi
     # Validate PROJECT_ROOT (must be absolute path)
     if [[ ! "$PROJECT_ROOT" =~ ^/ ]]; then
         log_error "PROJECT_ROOT must be absolute path: $PROJECT_ROOT"
         return 1
     fi
     ```
   - Use properly quoted sed substitutions (macOS sed with .bak extension)
   - Note: ametek_chess already has agent_name validation (line 314-318), keeping pattern

### Phase 3: Dependencies Setup

**Required Utilities** (copy from ametek_chess/scripts/lib/):
```
scripts/
├── worktree_manager.sh (main script)
├── lib/
│   ├── cross_platform_utils.sh (macOS utilities - Linux support not needed)
│   └── validation/
│       └── agent-names.sh (name validation, colors)
└── templates/
    ├── envrc.template (base template)
    └── agent-specific/ (optional overrides)
```

**Copy Commands**:
```bash
mkdir -p scripts/lib/validation scripts/templates/agent-specific
cp ~/projects/ametek_chess/scripts/lib/cross_platform_utils.sh scripts/lib/
cp ~/projects/ametek_chess/scripts/lib/validation/agent-names.sh scripts/lib/validation/
```

**Note**: macOS-only environment (cross-platform compatibility not required)

### Phase 4: Template Creation

**Base .envrc Template** (scripts/templates/envrc.template):
```bash
# Agent: {{AGENT_NAME}}
# Project: Euler_Problems
# Generated: $(date -Iseconds)

# Project root
export PROJECT_ROOT="{{PROJECT_ROOT}}"

# Agent isolation
export AGENT_NAME="{{AGENT_NAME}}"

# Build isolation
export CARGO_TARGET_DIR="$PROJECT_ROOT/target-{{AGENT_NAME}}"
export STACK_WORK_DIR="$PROJECT_ROOT/.stack-work-{{AGENT_NAME}}"

# Logging and temp
export LOG_DIR="$PROJECT_ROOT/logs/{{AGENT_NAME}}"
export TMPDIR="$PROJECT_ROOT/.tmp-{{AGENT_NAME}}"

# Path adjustments
PATH_add "$PROJECT_ROOT/scripts"

# Language-specific (Haskell, Rust, F#, etc.)
export HASKELL_DIST_DIR="$PROJECT_ROOT/dist-newstyle-{{AGENT_NAME}}"

# Euler-specific: no database, no web server, no ports
```

**Differences from ametek_chess template**:
- ❌ No DATABASE_URL
- ❌ No POSTGRES_* variables
- ❌ No AGENT_PORT (no listening services)
- ✅ Haskell-specific paths (.stack-work, dist-newstyle)
- ✅ Multi-language build dirs
- Template placeholders: {{AGENT_NAME}}, {{PROJECT_ROOT}} only (no {{PORT}})

### Phase 5: agent-coordination Skill Update

**Current Hardcoded Paths** (skill.md):
```
Line 25: Response: ametek-claude/.serena/memories/task-[id]-response.md
Line 42: if [ ! -d "ametek-<agent>" ]; then
Line 45:     # Creates: ametek-<agent>/ directory
Line 74: Database: ketema_chess_ephemeral_<agent>
Line 114: # Location: ametek-claude/.serena/memories/task-[id]-response.md
Line 123: # Write response: ametek-claude/.serena/memories/task-[id]-response.md
```

**Project-Agnostic Solution**:
Use environment variable `AGENT_WORKTREE_PREFIX` (set in .envrc):

```bash
# In Euler_Problems .envrc (main worktree):
export AGENT_WORKTREE_PREFIX="euler"

# In ametek_chess .envrc (main worktree):
export AGENT_WORKTREE_PREFIX="ametek"
```

**Updated Skill References**:
```bash
# OLD: ametek-claude/.serena/memories/task-[id]-response.md
# NEW: ${AGENT_WORKTREE_PREFIX}-claude/.serena/memories/task-[id]-response.md

# OLD: if [ ! -d "ametek-<agent>" ]; then
# NEW: if [ ! -d "${AGENT_WORKTREE_PREFIX}-<agent>" ]; then
```

**Database References** (conditional):
```bash
# In skill, add note:
# Database isolation: Only for projects with REQUIRES_DATABASE=true
# - ametek_chess: ketema_chess_ephemeral_<agent>
# - Euler_Problems: N/A (no databases)
```

### Phase 6: Memory Documentation

**Create**: `.serena/memories/project-worktree-configuration.md`

```markdown
# Euler_Problems Worktree Configuration

## Project-Specific Settings
- Worktree prefix: `euler`
- Port range: 30000-39999
- Database: None (REQUIRES_DATABASE=false)
- Build systems: Rust (Cargo), Haskell (Stack/Cabal), F#, etc.

## Agent Naming Examples
- euler-claude (Claude Code instance)
- euler-gemini (Gemini instance)
- euler-myclaude2 (secondary Claude instance)

## Comparison to ametek_chess
| Setting | ametek_chess | Euler_Problems |
|---------|--------------|----------------|
| Prefix | ametek | euler |
| Ports | 20000-29999 | 30000-39999 |
| Database | PostgreSQL | None |
| Build | Monorepo workspace | Per-problem isolation |

## Usage
```bash
# Create agent
./scripts/worktree_manager.sh create gemini

# List agents
./scripts/worktree_manager.sh list

# Health check
./scripts/worktree_manager.sh health

# Remove agent
./scripts/worktree_manager.sh remove gemini --force
```

## Future Expansion
If 3rd project requires worktree management:
1. Review this memory + ametek_chess configuration
2. Identify common patterns across 3 projects
3. Consider abstraction if patterns warrant (only after 3rd project)
4. YAGNI principle: Don't abstract prematurely
```

## Implementation Steps (Ordered)

1. **Create directory structure**
   ```bash
   mkdir -p scripts/lib/validation scripts/templates/agent-specific
   ```

2. **Copy dependencies from ametek_chess**
   ```bash
   cp ~/projects/ametek_chess/scripts/lib/cross_platform_utils.sh scripts/lib/
   cp ~/projects/ametek_chess/scripts/lib/validation/agent-names.sh scripts/lib/validation/
   ```

3. **Copy and adapt worktree_manager.sh**
   ```bash
   cp ~/projects/ametek_chess/scripts/worktree_manager.sh scripts/worktree_manager.sh
   # Edit: Remove DB functions, remove port functions, change "ametek" → "euler"
   # Add: set -euo pipefail at top (if not present)
   # Keep: Existing agent_name validation (line 314-318)
   # Add: PROJECT_ROOT validation in generate_envrc
   ```

4. **Create .envrc template**
   - Copy from ametek_chess template
   - Remove DATABASE_URL and PostgreSQL variables
   - Remove AGENT_PORT variable
   - Add Haskell-specific paths
   - Template placeholders: {{AGENT_NAME}}, {{PROJECT_ROOT}} only

5. **Initialize template system**
   ```bash
   ./scripts/worktree_manager.sh init-templates
   ```

6. **Update agent-coordination skill**
   - Replace hardcoded "ametek" paths with ${AGENT_WORKTREE_PREFIX}
   - Add conditional database documentation
   - Update examples for both projects

7. **Document in Serena memory**
   - Create project-worktree-configuration.md
   - Store comparison matrix
   - Document future expansion criteria (3rd project threshold)

8. **Comprehensive Testing**

   **8.1 Positive Tests**
   ```bash
   # Test: Create agent worktree
   ./scripts/worktree_manager.sh create myclaude2
   # Expected output: "Success: Agent myclaude2 workspace created at euler-myclaude2"
   # Expected exit code: 0
   # Expected filesystem state:
   #   - euler-myclaude2/ directory exists
   #   - euler-myclaude2/.envrc exists (no AGENT_PORT, no DATABASE_URL)
   #   - euler-myclaude2/.vscode/settings.json exists
   #   - logs/myclaude2/ directory exists
   #   - .tmp-myclaude2/ directory exists

   # Test: Health check
   ./scripts/worktree_manager.sh health
   # Expected output: "All checks passed - environment is healthy"
   # Expected exit code: 0
   # Expected checks:
   #   - "direnv is installed" (success)
   #   - "Agent myclaude2: .envrc exists" (success)
   #   - "Agent myclaude2: direnv allowed" (success)
   #   - Total: "0 issues" reported
   ```

   **8.2 Negative Tests**
   ```bash
   # Test: Invalid agent name (special characters)
   ./scripts/worktree_manager.sh create "invalid name!"
   # Expected output: "Error: Invalid agent name format: invalid name!"
   # Expected exit code: 1
   # Expected filesystem state: No euler-invalid* directory created

   # Test: Dirty worktree protection
   cd euler-myclaude2 && touch dirty.txt && git add dirty.txt && cd ..
   ./scripts/worktree_manager.sh remove myclaude2
   # Expected output: "Error: Worktree has uncommitted changes. Use --force to override or commit changes first."
   # Expected output: "Uncommitted files:" followed by "A  dirty.txt"
   # Expected exit code: 1
   # Expected filesystem state: euler-myclaude2/ still exists

   # Test: Force removal succeeds
   ./scripts/worktree_manager.sh remove myclaude2 --force
   # Expected output: "Agent myclaude2 workspace removed"
   # Expected exit code: 0
   # Expected filesystem state: euler-myclaude2/ directory removed
   ```

   **8.3 Security Tests**
   ```bash
   # Test: Command injection attempt in agent name
   ./scripts/worktree_manager.sh create 'agent$(whoami)'
   # Expected output: "Error: Invalid agent name format: agent$(whoami)"
   # Expected exit code: 1
   # Expected filesystem state: No euler-agent* directory created
   # Expected behavior: whoami command NOT executed (validation prevents injection)

   # Test: Path traversal attempt
   ./scripts/worktree_manager.sh create '../../../etc/passwd'
   # Expected output: "Error: Invalid agent name format: ../../../etc/passwd"
   # Expected exit code: 1
   # Expected filesystem state: No directories created outside project root

   # Test: Template substitution verification
   # After successful create, inspect .envrc:
   grep -E '\$\(|`|;|\||&' euler-myclaude2/.envrc
   # Expected output: Empty (no command substitution characters)
   # Expected exit code: 1 (grep finds nothing)
   # Validates: Template uses only validated AGENT_NAME and PROJECT_ROOT
   ```

   **8.4 Regression Tests** (run after any modifications)
   ```bash
   # Full lifecycle test
   ./scripts/worktree_manager.sh create testbot
   ./scripts/worktree_manager.sh list | grep euler-testbot
   ./scripts/worktree_manager.sh health
   ./scripts/worktree_manager.sh remove testbot --force
   git worktree list | grep euler-testbot
   # Expected: Last command exits 1 (worktree no longer listed)
   ```

9. **Update main .envrc**
   ```bash
   echo 'export AGENT_WORKTREE_PREFIX="euler"' >> .envrc
   direnv allow
   ```

## Success Criteria

✅ Script runs without database errors
✅ `create myclaude2` generates euler-myclaude2/ directory
✅ No AGENT_PORT in agent .envrc (removed)
✅ No DATABASE_URL in agent .envrc
✅ Health check passes for created agent
✅ agent-coordination skill works with ${AGENT_WORKTREE_PREFIX}
✅ Memory documents Euler vs ametek_chess differences
✅ Positive tests pass (create, health, remove)
✅ Negative tests properly reject invalid inputs
✅ Security tests validate input sanitization
✅ Template substitution uses validated inputs only
✅ set -euo pipefail present for error handling

## Rollback Plan

**If implementation fails during any phase**:

1. **Code Rollback**
   ```bash
   git status  # Check what was modified
   git reset --hard HEAD  # Discard all uncommitted changes
   # OR if changes were committed:
   git revert <commit-hash>  # Revert specific commit
   ```

2. **Worktree Cleanup**
   ```bash
   # List all euler-* worktrees
   git worktree list | grep euler-

   # Remove each worktree (forced if needed)
   git worktree remove --force euler-myclaude2
   git worktree remove --force euler-gemini
   # etc.

   # Prune orphaned references
   git worktree prune
   ```

3. **Directory Cleanup**
   ```bash
   # SAFETY: Validate paths before destructive operations
   if [[ -d "scripts" ]] && [[ "$(pwd)" == *"Euler_Problems" ]]; then
       rm -rf scripts/
       echo "Removed: scripts/"
   fi

   if [[ -d ".worktree-state" ]]; then
       rm -rf .worktree-state/
       echo "Removed: .worktree-state/"
   fi

   # Remove agent-specific artifacts (validate euler-* pattern)
   for dir in logs/* .tmp-* target-*; do
       if [[ -d "$dir" ]] && [[ "$dir" =~ (logs|\.tmp|target)-[a-zA-Z0-9_-]+$ ]]; then
           rm -rf "$dir"
           echo "Removed: $dir"
       fi
   done
   ```

4. **Skill Revert** (if agent-coordination skill was updated)
   ```bash
   # EXPLANATION: agent-coordination skill is SHARED across all projects
   # Location: ~/.claude/skills/agent-coordination/skill.md
   # Used by: ametek_chess, Euler_Problems, and potentially other projects
   # Changes made: Hardcoded "ametek-claude" paths → ${AGENT_WORKTREE_PREFIX}-claude

   # SAFETY: Check if skill was actually modified before resetting
   cd ~/.claude/skills/agent-coordination

   if git diff --quiet; then
       echo "Skill not modified - no reset needed"
   else
       echo "WARNING: Skill is shared across projects. Resetting will affect:"
       echo "  - ametek_chess agent coordination"
       echo "  - Euler_Problems agent coordination"
       echo "  - Any other projects using this skill"
       echo ""
       read -p "Proceed with skill reset? (y/N): " -n 1 -r
       echo
       if [[ $REPLY =~ ^[Yy]$ ]]; then
           # Create backup before reset
           git stash save "Rollback backup - $(date -Iseconds)"
           echo "Changes stashed. Restore with: git stash pop"

           # Show what will be reverted
           git diff HEAD

           # Reset skill
           git reset --hard HEAD
           echo "Skill reset complete"
       else
           echo "Skill reset skipped - manual review required"
       fi
   fi
   ```

   **Why This Matters**:
   - The agent-coordination skill lives in ~/.claude/skills/ (Claude Code's global skill directory)
   - It's NOT project-specific - ALL projects use the same skill file
   - Phase 4 of implementation updates skill.md to use ${AGENT_WORKTREE_PREFIX} variable
   - This makes the skill work for BOTH ametek_chess AND Euler_Problems
   - Reverting the skill would break agent coordination for ALL projects, not just Euler
   - **Recommendation**: Only revert skill if BOTH projects need rollback, otherwise leave skill changes in place

5. **Environment Cleanup**
   ```bash
   # Remove AGENT_WORKTREE_PREFIX from main .envrc if added
   # Edit .envrc manually to remove the line
   direnv allow  # Refresh environment
   ```

6. **Verification**
   ```bash
   git worktree list  # Should show no euler-* worktrees
   ls -la scripts/  # Should not exist (or restored state)
   git log --oneline -5  # Verify clean state
   ```

7. **Document Failure**
   ```bash
   # Create failure analysis in memory
   echo "## Worktree Implementation Failure - $(date)" >> .serena/memories/worktree-failures.md
   echo "Reason: [describe what failed]" >> .serena/memories/worktree-failures.md
   echo "Next steps: [what to try differently]" >> .serena/memories/worktree-failures.md
   ```

**Rollback Triggers**:
- Script errors during adaptation (syntax, missing functions)
- Template security validation failures
- Test failures (positive, negative, or security tests)
- agent-coordination skill integration issues
- File permission or direnv errors

## AI Panel Feedback Integration

**Review**: Submitted to AI Panel (conversation cf5bce47, 2025-01-18)
**Score**: 6/10 (Medium Complexity, Medium Feasibility)
**Model**: gemini-2.5-flash (Google)

**Strengths Identified**:
- ✅ Rigorous YAGNI compliance
- ✅ Correct abstraction threshold (copy+adapt for 2 projects)
- ✅ Excellent documentation strategy
- ✅ Well-defined cross-project coordination

**Critical Issues Addressed**:

1. **Template Security** (HIGH - Medium Severity)
   - Issue: sed-based substitution vulnerable to shell injection
   - Solution: Added input validation in `generate_envrc` (lines 122-136)
   - Validates agent_name format (alphanumeric + hyphen/underscore)
   - Validates PROJECT_ROOT is absolute path
   - Note: ametek_chess already has agent_name validation (line 314-318)

2. **Testing Strategy** (HIGH - Medium Severity)
   - Issue: Missing negative tests, security tests
   - Solution: Added comprehensive testing (step 8, lines 334-350)
   - Positive: create, health, remove
   - Negative: invalid names, dirty worktree protection
   - Security: injection attempts validation

3. **Rollback Plan** (MEDIUM - Low Severity)
   - Issue: No formal rollback beyond git revert
   - Solution: 7-step rollback procedure (lines 373-448)
   - Covers: code, worktrees, directories, skill, environment
   - Includes verification and failure documentation

4. **Error Handling** (MEDIUM - Low Severity)
   - Issue: No mention of set -euo pipefail
   - Solution: Added to step 3 (line 307) and success criteria (line 371)

**Cross-Platform Note**:
- User confirmed macOS-only environment
- Cross-platform compatibility not required (removed from concerns)
- cross_platform_utils.sh retained but Linux compatibility not tested

**Second Iteration Refinements** (addressing 3 remaining risks):

1. **Rollback rm -rf Safety** (HIGH - Implemented)
   - Issue: Variables could be empty/malformed, deleting unintended files
   - Solution: Added path validation before all rm -rf operations (lines 401-419)
   - Guards: pwd check for Euler_Problems, directory existence, pattern matching for agent artifacts
   - Example: `if [[ -d "scripts" ]] && [[ "$(pwd)" == *"Euler_Problems" ]]; then`

2. **Test Automation Clarity** (MEDIUM - Implemented)
   - Issue: Test commands didn't specify expected outputs/exit codes
   - Solution: Expanded testing section with expected results (lines 336-415)
   - Coverage: Expected output, exit codes, filesystem state for each test
   - Categories: Positive (8.1), Negative (8.2), Security (8.3), Regression (8.4)

3. **Skill Directory Reset** (MEDIUM - Documented + Safeguarded)
   - Issue: ~/.claude/skills/agent-coordination is SHARED across all projects
   - Explanation: Skill is global, not project-specific (lines 458-464)
   - Impact: Resetting skill breaks agent coordination for ametek_chess AND Euler_Problems
   - Solution: Added user confirmation prompt + git stash backup (lines 431-455)
   - Recommendation: Only revert skill if BOTH projects need rollback

## Token Efficiency Notes

- Copy + adapt cheaper than abstract + configure (YAGNI validated)
- Environment variable approach (${AGENT_WORKTREE_PREFIX}) adds 0 complexity
- Two-project comparison documented for free 3rd-project evaluation
- AI Panel feedback integrated: +5 success criteria, 7-step rollback, security hardening
