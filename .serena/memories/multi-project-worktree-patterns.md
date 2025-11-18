# Multi-Project Worktree Management Patterns

**Purpose**: Document similarities and differences across projects using multi-agent worktree infrastructure
**Projects**: ametek_chess (baseline), Euler_Problems (current), [future 3rd project]
**Created**: 2025-01-18
**Update Policy**: Add new project columns when 3rd project requires worktree management

## Configuration Matrix

| Configuration | ametek_chess | Euler_Problems | [3rd Project] |
|---------------|--------------|----------------|---------------|
| **Worktree Prefix** | `ametek` | `euler` | TBD |
| **Port Range** | 20000-29999 (web servers) | ❌ N/A (no services) | TBD |
| **Database Required** | ✅ Yes (PostgreSQL) | ❌ No | TBD |
| **Database Name Pattern** | `ketema_chess_ephemeral_<agent>` | N/A | TBD |
| **Template Database** | `ketema_chess_dev_ephemeral` | N/A | TBD |
| **Build System** | Rust (Cargo workspace), Python (Poetry), Haskell (Stack) | Rust, Haskell, F#, etc. (per-problem) | TBD |
| **Project Structure** | Monorepo (shared workspace) | Problem-based (isolated builds) | TBD |
| **Schema Migrations** | Auto-applied (SQLx/Alembic) | N/A | TBD |
| **Dependency Sync** | Cross-worktree sync needed | Per-problem isolation | TBD |
| **Script Location** | `scripts/worktree_manager.sh` | `scripts/worktree_manager.sh` | TBD |

## Common Patterns (Universal)

These elements are **identical** across all projects and should be preserved when creating new worktree infrastructure:

### 1. Core Worktree Lifecycle
- `create <agent>` - Create agent worktree with full environment
- `remove <agent> [--force]` - Remove worktree (safety checks)
- `list` - Show all agent worktrees
- `status` - Health check + disk usage
- `cleanup` - Prune orphaned references

### 2. Environment Isolation (Agent-Specific)
```bash
# Every project provides these per agent:
export AGENT_NAME="<agent>"
export CARGO_TARGET_DIR="target-<agent>"
export LOG_DIR="logs/<agent>"
export TMPDIR=".tmp-<agent>"

# Conditional (only if REQUIRES_PORTS=true):
export AGENT_PORT=<deterministic-hash-based>
```

### 3. Template System
- `init-templates` - One-time setup from existing .envrc
- `sync-envrc` - Update all agents from template
- `backup [path]` - Safety before modifications
- `restore [path]` - Rollback from backup
- Placeholders (universal): `{{AGENT_NAME}}`, `{{PROJECT_ROOT}}`
- Placeholders (conditional): `{{PORT}}` (only if REQUIRES_PORTS=true)

### 4. Validation & Safety
- Agent name validation (alphanumeric + hyphen/underscore)
- Dirty worktree protection (refuse removal unless --force)
- Template security checks (no dangerous patterns)
- Sentinel respect (main .envrc protected unless FORCE_SYNC_MAIN_ENVRC=1)

### 5. State Tracking
```bash
.worktree-state/
├── sync-<agent>.timestamp       # Last .envrc sync
├── active-<agent>.state         # Agent activity
└── hook-<agent>.status          # Hook status
```

### 6. Cross-Platform Support
- macOS vs Linux sed differences handled
- Shell detection (bash vs zsh)
- direnv hook installation
- Python3 for port generation

## Project-Specific Patterns

### Database Management (Conditional)

**When Needed** (ametek_chess):
```bash
# Functions to include:
create_agent_database()
remove_agent_database()
list_agent_databases()
cleanup_orphan_databases()

# Configuration:
readonly REQUIRES_DATABASE=true
readonly DB_NAME_PREFIX="ketema_chess_ephemeral"
readonly TEMPLATE_DB="ketema_chess_dev_ephemeral"
export POSTGRES_HOST="${POSTGRES_HOST:-localhost}"
export POSTGRES_PORT="${POSTGRES_PORT:-5432}"
export POSTGRES_USER="${POSTGRES_USER:-ketema}"
```

**When NOT Needed** (Euler_Problems):
```bash
# Configuration:
readonly REQUIRES_DATABASE=false

# Remove these functions entirely:
# - create_agent_database (lines 682-714)
# - remove_agent_database (lines 717-746)
# - list_agent_databases (lines 749-760)
# - cleanup_orphan_databases (lines 763-816)
```

### Build System Isolation

**Monorepo (ametek_chess)**:
```bash
# Unified workspace - sync across agents
sync_dependencies() {
    # Update Poetry, Cargo, Stack in all worktrees
    for worktree in ametek-*; do
        cd "$worktree"
        cd python && poetry lock --no-update
        cd rust && cargo generate-lockfile
        cd haskell && stack build --only-dependencies
    done
}
```

**Per-Problem (Euler_Problems)**:
```bash
# No cross-agent sync needed
# Each problem/ dir has independent build
# Remove sync_dependencies() function entirely
```

### Port Assignment Strategy

**Only for projects with REQUIRES_PORTS=true**

**Formula**: `PORT_BASE + hash(agent_name) % RANGE_SIZE`

```bash
# ametek_chess (web servers, database connections, monitoring):
PORT_BASE=20000
RANGE_SIZE=10000
# Result: 20000-29999

# Euler_Problems:
REQUIRES_PORTS=false  # No listening services

# [3rd project with services]:
PORT_BASE=40000
RANGE_SIZE=10000
# Result: 40000-49999 (proposed)
```

**Rationale**:
- Non-overlapping ranges prevent conflicts when running agents from multiple projects simultaneously
- Only assign ports if project actually needs them (YAGNI)
- Euler_Problems has no web servers, databases, or listening services → no ports needed

## agent-coordination Skill Adaptation

### Problem: Hardcoded Project Paths

**Original (ametek_chess only)**:
```bash
# Line 25, 42, 45, 74, 114, 123, etc.
ametek-claude/.serena/memories/task-[id]-response.md
ketema_chess_ephemeral_<agent>
```

### Solution: Environment Variable Prefix

**Universal Pattern**:
```bash
# Each project's main .envrc:
export AGENT_WORKTREE_PREFIX="<project-prefix>"

# ametek_chess:
export AGENT_WORKTREE_PREFIX="ametek"

# Euler_Problems:
export AGENT_WORKTREE_PREFIX="euler"

# Skill references:
${AGENT_WORKTREE_PREFIX}-claude/.serena/memories/task-[id]-response.md
```

**Conditional Documentation**:
```markdown
## Environment Isolation (automatic per worktree)
- Database: `${DB_NAME_PREFIX}_<agent>` (if REQUIRES_DATABASE=true)
- Build: `target-<agent>` (no artifact conflicts)
- Logs: `logs/<agent>` (separate logging)
- Temp: `.tmp-<agent>` (isolated temporary files)
- Port: Deterministic hash-based (project-specific range)
```

## Abstraction Threshold (YAGNI Principle)

**Current State**: 2 projects, copy + adapt approach
**Justification**: Different enough to warrant separate scripts (database vs no-database is fundamental)

**When to Abstract** (3rd project triggers):
1. If 3rd project has database needs → 2/3 have DB → abstract DB layer
2. If 3rd project has no database → 2/3 no DB → abstract no-DB pattern
3. If 3rd project introduces NEW pattern → reevaluate architecture

**Abstraction Design** (if triggered):
```bash
# Option A: Configuration file per project
scripts/
├── worktree_manager.sh (generic engine)
└── config/
    ├── ametek_chess.conf
    ├── euler_problems.conf
    └── project3.conf

# Option B: Shared library + project stubs
scripts/
├── lib/
│   └── worktree_core.sh (common functions)
├── ametek_worktree.sh (project-specific)
├── euler_worktree.sh (project-specific)
└── project3_worktree.sh (project-specific)

# Option C: Plugin architecture
scripts/
├── worktree_manager.sh (core)
└── plugins/
    ├── database.sh (if REQUIRES_DATABASE)
    ├── monorepo.sh (if sync_dependencies)
    └── isolated.sh (if per-problem)
```

**Decision Criteria**:
- If common code > 80% across 3 projects → Option A (config-driven)
- If common code 50-80% → Option B (shared lib)
- If common code < 50% → Keep separate (YAGNI still applies)

## Reference for Future Projects

### Checklist for New Project Worktree Setup

1. **Identify requirements**:
   - [ ] Database needed? (PostgreSQL, MySQL, etc.)
   - [ ] Build system? (Cargo workspace, per-module, monorepo)
   - [ ] Port range available? (suggest next 10K block)
   - [ ] Special environment needs? (API keys, cloud credentials)

2. **Choose implementation approach**:
   - [ ] If <3 projects: Copy + adapt from closest match
   - [ ] If 3+ projects: Review abstraction threshold criteria
   - [ ] Document decision in this memory

3. **Create project-specific config**:
   - [ ] Worktree prefix (short, unique)
   - [ ] Port range (non-overlapping)
   - [ ] Database pattern (if needed)
   - [ ] Build isolation strategy

4. **Copy dependencies**:
   - [ ] cross_platform_utils.sh
   - [ ] validation/agent-names.sh
   - [ ] Adapt worktree_manager.sh

5. **Test lifecycle**:
   - [ ] Create test agent
   - [ ] Verify environment isolation
   - [ ] Run health check
   - [ ] Remove test agent
   - [ ] Update this memory with findings

## Lessons Learned

### ametek_chess → Euler_Problems

**What Worked**:
- Dynamic PROJECT_ROOT discovery (git rev-parse) - portable
- Deterministic port hashing - prevents conflicts
- Template system with placeholders - maintainable
- State tracking (.worktree-state/) - debugging aid
- Health check comprehensiveness - catches issues early

**What Changed**:
- Database functions removed (50% smaller script)
- Port range shifted (prevent runtime conflicts)
- sync_dependencies removed (not applicable)
- Worktree prefix changed (obvious in git worktree list)

**What Failed** (before fix):
- Hardcoded "ametek" in agent-coordination skill → now ${AGENT_WORKTREE_PREFIX}
- Assumed database presence → now conditional documentation
- Assumed monorepo → now documented per-project structure

### Future Improvements (Apply to All Projects)

1. **Make port range configurable** in script header (currently requires code edit)
2. **Add project detection** via git remote origin (auto-set prefix)
3. **Validate port availability** before assignment (currently warns only)
4. **Add telemetry** for agent lifecycle (created when, last active, total uptime)
5. **Cross-project agent list** (show all euler-*, ametek-*, etc. in one view)

## Version History

- **2025-01-18**: Initial documentation (ametek_chess baseline, Euler_Problems addition)
- **[Future]**: 3rd project evaluation and potential abstraction decision
