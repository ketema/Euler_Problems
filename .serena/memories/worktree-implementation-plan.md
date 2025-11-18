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
   - Keep AGENT_NAME and PROJECT_ROOT substitutions

### Phase 3: Dependencies Setup

**Required Utilities** (copy from ametek_chess/scripts/lib/):
```
scripts/
├── worktree_manager.sh (main script)
├── lib/
│   ├── cross_platform_utils.sh (macOS/Linux compatibility)
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
   # Edit: Remove DB functions, change "ametek" → "euler", port range 30000-39999
   ```

4. **Create .envrc template**
   - Copy from ametek_chess template
   - Remove DATABASE_URL and PostgreSQL variables
   - Add Haskell-specific paths
   - Update port range

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

8. **Test with myclaude2 agent**
   ```bash
   ./scripts/worktree_manager.sh create myclaude2
   ./scripts/worktree_manager.sh health
   # Verify: euler-myclaude2/ exists, .envrc correct, no DB errors
   ```

9. **Update main .envrc**
   ```bash
   echo 'export AGENT_WORKTREE_PREFIX="euler"' >> .envrc
   direnv allow
   ```

## Success Criteria

✅ Script runs without database errors
✅ `create myclaude2` generates euler-myclaude2/ directory
✅ .envrc has port in 30000-39999 range
✅ No DATABASE_URL in agent .envrc
✅ Health check passes for created agent
✅ agent-coordination skill works with ${AGENT_WORKTREE_PREFIX}
✅ Memory documents Euler vs ametek_chess differences

## Rollback Plan

If implementation fails:
1. Remove scripts/ directory
2. Remove created worktrees: `git worktree remove euler-*`
3. Restore from ametek_chess if needed
4. Document failure reason in memory for next attempt

## Token Efficiency Notes

- Copy + adapt cheaper than abstract + configure (YAGNI validated)
- Environment variable approach (${AGENT_WORKTREE_PREFIX}) adds 0 complexity
- Two-project comparison documented for free 3rd-project evaluation
