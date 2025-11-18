#!/usr/bin/env bash
# scripts/worktree_manager.sh
# Multi-Agent Worktree Management Script
# Phase 2: Core Worktree Infrastructure Implementation

set -euo pipefail

# Dynamic project root discovery for portability
readonly PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || { cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd; })"
readonly SCRIPT_DIR="$PROJECT_ROOT/scripts"

# Robust utility sourcing function
source_utility() {
    local utility_name="$1"
    local search_paths=(
        "$SCRIPT_DIR/lib/${utility_name}"
        "$PROJECT_ROOT/lib/${utility_name}"
        "$SCRIPT_DIR/${utility_name}"
    )

    for path in "${search_paths[@]}"; do
        if [[ -f "$path" ]]; then
            # shellcheck source=/dev/null
            source "$path"
            return 0
        fi
    done

    echo "Error: Cannot find required utility: $utility_name" >&2
    echo "Searched paths:" >&2
    printf "  - %s\n" "${search_paths[@]}" >&2
    exit 1
}

# Load dependencies with robust path resolution
source_utility "cross_platform_utils.sh"
source_utility "lib/validation/agent-names.sh"

# Note: MAX_AGENT_NAME_LENGTH, MIN_AGENT_NAME_LENGTH, and colors are defined in agent-names.sh

# Logging functions
log_info() {
    echo -e "${BLUE}Info: $1${NC}" >&2
}

log_success() {
    echo -e "${GREEN}Success: $1${NC}" >&2
}

log_error() {
    echo -e "${RED}Error: $1${NC}" >&2
}

log_warning() {
    echo -e "${YELLOW}Warning: $1${NC}" >&2
}

# List all active worktrees
list_agents() {
    echo "=== Active Agent Worktrees ==="
    git worktree list
}

# Create new agent worktree
create_agent() {
    local agent_name=$1
    local base_branch=${2:-main}
    
    if [ -z "$agent_name" ]; then
        log_error "Usage: create_agent <agent-name> [base-branch]"
        return 1
    fi
    
    # Validate agent name using existing validation from issue #90
    if ! validate_agent_name "$agent_name"; then
        return 1
    fi
    
    local worktree_dir="euler-$agent_name"
    
    # Check if worktree already exists
    if [ -d "$worktree_dir" ]; then
        log_error "Worktree $worktree_dir already exists"
        return 1
    fi
    
    log_info "Creating worktree for agent: $agent_name"

    # Create worktree with a unique branch for this agent
    # This allows multiple agents to work independently
    local agent_branch="$agent_name-main"

    if ! git worktree add "$worktree_dir" -b "$agent_branch" "$base_branch"; then
        log_error "Failed to create worktree $worktree_dir"
        return 1
    fi
    
    # Setup isolated environment for the agent
    if ! setup_agent_environment "$agent_name"; then
        log_error "Failed to setup environment for agent $agent_name"
        # Cleanup failed worktree
        git worktree remove "$worktree_dir" 2>/dev/null || true
        return 1
    fi

    log_success "Agent $agent_name workspace created at $worktree_dir"
}

# Setup isolated environment for agent
setup_agent_environment() {
    local agent_name=$1
    local worktree_dir="euler-$agent_name"

    if [[ ! -d "$worktree_dir" ]]; then
        log_error "Worktree directory $worktree_dir does not exist"
        return 1
    fi

    log_info "Setting up environment for agent: $agent_name"

    # Check prerequisites
    if ! command -v python3 >/dev/null 2>&1; then
        log_error "python3 required for port generation"
        return 1
    fi

    cd "$worktree_dir" || {
        log_error "Cannot access worktree directory $worktree_dir"
        return 1
    }

    # Create required directories with error checking
    mkdir -p "logs/$agent_name" ".tmp-$agent_name" || {
        log_error "Cannot create required directories"
        return 1
    }

    # Note: Port generation and testing removed - Euler_Problems has no listening services

    # Create environment configuration via template pipeline
    generate_envrc "$agent_name" "$PWD" || {
        log_error "generate_envrc failed for $agent_name"
        return 1
    }
    
    # Create VS Code settings
    mkdir -p .vscode
    cat > .vscode/settings.json << EOF
{
  "rust-analyzer.cargo.targetDir": "target-$agent_name",
  "python.defaultInterpreterPath": "./.venv/bin/python",
  "terminal.integrated.env.linux": {
    "CARGO_TARGET_DIR": "target-$agent_name",
    "LOG_DIR": "logs/$agent_name",
    "TMPDIR": ".tmp-$agent_name"
  },
  "terminal.integrated.env.osx": {
    "CARGO_TARGET_DIR": "target-$agent_name",
    "LOG_DIR": "logs/$agent_name",
    "TMPDIR": ".tmp-$agent_name"
  },
  "git.detectSubmodules": false
}
EOF

    cd - > /dev/null
    log_success "Environment setup completed for agent: $agent_name"
}

# Setup direnv hook for shell compatibility
setup_direnv_hook() {
    local shell_type config_file hook_command
    
    # Detect current shell
    if [[ -n "${ZSH_VERSION:-}" ]]; then
        shell_type="zsh"
        config_file="$HOME/.zshrc"
        hook_command='eval "$(direnv hook zsh)"'
    elif [[ -n "${BASH_VERSION:-}" ]]; then
        shell_type="bash"
        config_file="$HOME/.bashrc"
        hook_command='eval "$(direnv hook bash)"'
    else
        # Fallback: check $SHELL
        case "${SHELL:-}" in
            */zsh) 
                shell_type="zsh"
                config_file="$HOME/.zshrc"
                hook_command='eval "$(direnv hook zsh)"'
                ;;
            */bash)
                shell_type="bash"
                config_file="$HOME/.bashrc"
                hook_command='eval "$(direnv hook bash)"'
                ;;
            *)
                log_error "Unsupported shell: ${SHELL:-unknown}. Please use bash or zsh."
                return 1
                ;;
        esac
    fi
    
    # Check if direnv is installed
    if ! command -v direnv >/dev/null 2>&1; then
        log_error "direnv is not installed. Please install direnv first."
        log_info "Install with: brew install direnv (macOS) or apt install direnv (Linux)"
        return 1
    fi
    
    # Add hook if not already present
    if ! grep -q "direnv hook" "$config_file" 2>/dev/null; then
        echo "" >> "$config_file"
        echo "# Direnv hook for multi-agent worktree isolation" >> "$config_file"
        echo "$hook_command" >> "$config_file"
        log_success "Added direnv hook to $config_file ($shell_type)"
        log_warning "Please restart your shell or run: source $config_file"
        return 0
    else
        log_info "Direnv hook already configured in $config_file"
        return 0
    fi
}

# Note: Port generation removed - Euler_Problems has no listening services

# Generate .envrc from template
generate_envrc() {
    local agent_name=$1
    local worktree_dir=$2
    
    # Input validation
    [[ -z "$agent_name" ]] && { log_error "Agent name required"; return 1; }
    [[ ! -d "$worktree_dir" ]] && { log_error "Invalid worktree directory: $worktree_dir"; return 1; }
    
    log_info "Generating .envrc for agent: $agent_name"
    
    # Check if template exists
    if [[ ! -f "$SCRIPT_DIR/templates/envrc.template" ]]; then
        log_error "Template not found: $SCRIPT_DIR/templates/envrc.template"
        log_info "Run: $0 init-templates"
        return 1
    fi
    
    # Validate template security
    if ! validate_template "$SCRIPT_DIR/templates/envrc.template"; then
        log_error "Template validation failed - security check"
        return 1
    fi
    
    # Create backup of existing .envrc
    if ! backup_envrc "$worktree_dir"; then
        log_warning "Backup failed but continuing"
    fi
    
    # Choose template: orchestrator for main, base for agents
    local template_src="$SCRIPT_DIR/templates/envrc.template"
    if [[ "$agent_name" == "main" ]] && [[ -f "$SCRIPT_DIR/templates/orchestrator.envrc.template" ]]; then
        template_src="$SCRIPT_DIR/templates/orchestrator.envrc.template"
    fi

    # Respect sentinel in existing main .envrc: skip unless forced via env FLAG
    if [[ "$agent_name" == "main" ]] && [[ -f "$worktree_dir/.envrc" ]] && grep -q "NOT be overwritten" "$worktree_dir/.envrc" && [[ -z "${FORCE_SYNC_MAIN_ENVRC:-}" ]]; then
        log_info "Skipping main .envrc generation (sentinel present). Set FORCE_SYNC_MAIN_ENVRC=1 to override."
        update_state "sync" "$agent_name"
        return 0
    fi

    # Copy chosen template
    if ! cp "$template_src" "$worktree_dir/.envrc"; then
        log_error "Failed to copy template to $worktree_dir/.envrc"
        restore_envrc "$worktree_dir"
        return 1
    fi

    # Validate agent name (prevent injection)
    if [[ ! "$agent_name" =~ ^[a-zA-Z0-9_-]+$ ]]; then
        log_error "Invalid agent name format: $agent_name"
        restore_envrc "$worktree_dir"
        return 1
    fi

    # Apply agent-specific substitutions (macOS only)
    sed -i.bak "s|{{AGENT_NAME}}|$agent_name|g" "$worktree_dir/.envrc"
    sed -i.bak "s|{{PROJECT_ROOT}}|$PROJECT_ROOT|g" "$worktree_dir/.envrc"
    rm "$worktree_dir/.envrc.bak"
    
    # Append agent-specific variables if they exist
    # Note: .override extension to avoid .gitignore
    if [[ -f "$SCRIPT_DIR/templates/agent-specific/${agent_name}.override" ]]; then
        echo "" >> "$worktree_dir/.envrc"
        echo "# Agent-specific configuration" >> "$worktree_dir/.envrc"
        cat "$SCRIPT_DIR/templates/agent-specific/${agent_name}.override" >> "$worktree_dir/.envrc"
    fi
    
    # Set secure permissions
    chmod 644 "$worktree_dir/.envrc"
    
    # Allow direnv
    if (cd "$worktree_dir" && direnv allow 2>/dev/null); then
        log_success "direnv allowed for $worktree_dir"
    else
        log_warning "Could not auto-allow direnv - manual 'direnv allow' may be needed"
    fi
    
    # Update state tracking
    update_state "sync" "$agent_name"

    log_success "Generated .envrc for $agent_name"
    return 0
}

# Sync .envrc files from templates
sync_envrc() {
    log_info "Synchronizing .envrc files from templates..."
    
    # Check if template exists
    if [[ ! -f "$SCRIPT_DIR/templates/envrc.template" ]]; then
        log_error "Template not found: $SCRIPT_DIR/templates/envrc.template"
        log_error "Please create the template first or run: $0 init-templates"
        return 1
    fi
    
    # Main worktree (if not in a git directory)
    if [[ -d ".git" ]]; then
        # We're in the main worktree
        generate_envrc "main" "."
    fi
    
    # Agent worktrees
    for worktree in euler-*; do
        if [[ -d "$worktree" ]]; then
            local agent_name=$(basename "$worktree" | sed 's/euler-//')
            generate_envrc "$agent_name" "$worktree"
        fi
    done
    
    log_success "All .envrc files synchronized from templates"
}

# Initialize template system (one-time setup)
init_templates() {
    log_info "Initializing .envrc template system..."
    
    # Create template directories if they don't exist
    mkdir -p "$SCRIPT_DIR/templates/agent-specific"
    
    # Check if template already exists
    if [[ -f "$SCRIPT_DIR/templates/envrc.template" ]]; then
        log_warning "Template already exists: $SCRIPT_DIR/templates/envrc.template"
        read -p "Overwrite existing template? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            log_info "Keeping existing template"
            return 0
        fi
    fi
    
    # Create template from existing .envrc if available
    local source_envrc=""
    if [[ -f "euler-claude/.envrc" ]]; then
        source_envrc="euler-claude/.envrc"
    elif [[ -f ".envrc" ]]; then
        source_envrc=".envrc"
    fi
    
    if [[ -n "$source_envrc" ]]; then
        log_info "Creating template from: $source_envrc"
        cp "$source_envrc" "$SCRIPT_DIR/templates/envrc.template"
        
        # Replace specific values with placeholders
        if [[ "$OSTYPE" == "darwin"* ]]; then
            sed -i.bak 's/claude/{{AGENT_NAME}}/g' "$SCRIPT_DIR/templates/envrc.template"
            sed -i.bak 's/25767/{{PORT}}/g' "$SCRIPT_DIR/templates/envrc.template"
            sed -i.bak "s|$PROJECT_ROOT|{{PROJECT_ROOT}}|g" "$SCRIPT_DIR/templates/envrc.template"
            rm "$SCRIPT_DIR/templates/envrc.template.bak"
        else
            sed -i 's/claude/{{AGENT_NAME}}/g' "$SCRIPT_DIR/templates/envrc.template"
            sed -i 's/25767/{{PORT}}/g' "$SCRIPT_DIR/templates/envrc.template"
            sed -i "s|$PROJECT_ROOT|{{PROJECT_ROOT}}|g" "$SCRIPT_DIR/templates/envrc.template"
        fi
    else
        log_warning "No existing .envrc found. Template created but may need manual editing."
    fi
    
    log_success "Template system initialized"
    log_info "Edit templates in: $SCRIPT_DIR/templates/"
}

# Validate template content for security
validate_template() {
    local template_file=$1
    
    [[ -z "$template_file" ]] && { log_error "Template file required"; return 1; }
    [[ ! -f "$template_file" ]] && { log_error "Template not found: $template_file"; return 1; }
    
    # Check for dangerous commands
    local dangerous_patterns=(
        'rm -rf'
        'curl.*\|.*sh'
        'wget.*\|.*sh'
        'eval'
        '`.*`'
        '\$\(.*\)'
    )
    
    for pattern in "${dangerous_patterns[@]}"; do
        if grep -qE "$pattern" "$template_file" 2>/dev/null; then
            log_error "Dangerous pattern detected in template: $pattern"
            return 1
        fi
    done
    
    # Validate placeholders
    local required_placeholders=(
        '{{AGENT_NAME}}'
        '{{PORT}}'
        '{{PROJECT_ROOT}}'
    )
    
    for placeholder in "${required_placeholders[@]}"; do
        if ! grep -qF "$placeholder" "$template_file"; then
            log_warning "Missing placeholder in template: $placeholder"
        fi
    done
    
    return 0
}

# Backup .envrc before modifications
backup_envrc() {
    local worktree_dir=$1
    local backup_dir="$PROJECT_ROOT/.envrc-backups"
    
    [[ -z "$worktree_dir" ]] && { log_error "Worktree directory required"; return 1; }
    
    # Create backup directory if needed
    mkdir -p "$backup_dir"
    
    # Only backup if .envrc exists
    if [[ -f "$worktree_dir/.envrc" ]]; then
        local timestamp=$(date +%Y%m%d-%H%M%S)
        local agent_name=$(basename "$worktree_dir" | sed 's/euler-//')
        [[ "$worktree_dir" == "." ]] && agent_name="main"
        
        local backup_file="$backup_dir/${timestamp}-${agent_name}.envrc.bak"
        
        if cp "$worktree_dir/.envrc" "$backup_file"; then
            log_info "Backed up .envrc to: $backup_file"
            return 0
        else
            log_error "Backup failed for $worktree_dir/.envrc"
            return 1
        fi
    fi
    
    return 0
}

# Restore .envrc from backup
restore_envrc() {
    local worktree_dir=$1
    local backup_dir="$PROJECT_ROOT/.envrc-backups"
    
    [[ -z "$worktree_dir" ]] && { log_error "Worktree directory required"; return 1; }

    local agent_name=$(basename "$worktree_dir" | sed 's/euler-//')
    [[ "$worktree_dir" == "." ]] && agent_name="main"
    
    # Find most recent backup
    local latest_backup=$(ls -t "$backup_dir"/*-"${agent_name}".envrc.bak 2>/dev/null | head -1)
    
    if [[ -n "$latest_backup" ]] && [[ -f "$latest_backup" ]]; then
        if cp "$latest_backup" "$worktree_dir/.envrc"; then
            log_success "Restored .envrc from: $latest_backup"
            return 0
        else
            log_error "Restore failed from: $latest_backup"
            return 1
        fi
    else
        log_error "No backup found for agent: $agent_name"
        return 1
    fi
}

# Update state tracking
update_state() {
    local state_type=$1
    local agent_name=$2
    local value=${3:-}

    local state_dir="$PROJECT_ROOT/.worktree-state"
    mkdir -p "$state_dir"

    case "$state_type" in
        "sync")
            echo "$(date -Iseconds)" > "$state_dir/sync-${agent_name}.timestamp"
            ;;
        "hook")
            echo "$value" > "$state_dir/hook-${agent_name}.status"
            ;;
        "active")
            echo "$(date -Iseconds)" > "$state_dir/active-${agent_name}.state"
            ;;
        *)
            log_warning "Unknown state type: $state_type"
            ;;
    esac
}

# Health check for worktree environment
health_check() {
    log_info "Performing worktree environment health check..."
    
    local issues=0
    
    # Check direnv installation
    if ! command -v direnv >/dev/null 2>&1; then
        log_error "direnv is not installed"
        ((issues++))
    else
        log_success "direnv is installed"
    fi
    
    # Check shell hooks
    for shell_rc in "$HOME/.bashrc" "$HOME/.zshrc"; do
        if [[ -f "$shell_rc" ]]; then
            if grep -q "direnv hook" "$shell_rc"; then
                log_success "direnv hook found in: $shell_rc"
            else
                log_warning "direnv hook missing in: $shell_rc"
                ((issues++))
            fi
        fi
    done
    
    # Check templates
    if [[ -f "$SCRIPT_DIR/templates/envrc.template" ]]; then
        if validate_template "$SCRIPT_DIR/templates/envrc.template"; then
            log_success "Template validation passed"
        else
            log_error "Template validation failed"
            ((issues++))
        fi
    else
        log_error "Template not found: $SCRIPT_DIR/templates/envrc.template"
        ((issues++))
    fi
    
    # Check each worktree
    for worktree in euler-*; do
        if [[ -d "$worktree" ]]; then
            local agent_name=$(basename "$worktree" | sed 's/euler-//')
            
            if [[ -f "$worktree/.envrc" ]]; then
                log_success "Agent $agent_name: .envrc exists"
                
                # Check if direnv is allowed
                if (cd "$worktree" && direnv status 2>&1 | grep -q "Allowed: true"); then
                    log_success "Agent $agent_name: direnv allowed"
                else
                    log_warning "Agent $agent_name: direnv not allowed"
                    ((issues++))
                fi
            else
                log_warning "Agent $agent_name: .envrc missing"
                ((issues++))
            fi
        fi
    done
    
    # Summary
    echo "=== Health Check Summary ==="
    if [[ $issues -eq 0 ]]; then
        log_success "All checks passed - environment is healthy"
        return 0
    else
        log_error "Found $issues issues - please run setup commands"
        return 1
    fi
}

# Remove completed agent worktree
remove_agent() {
    local agent_name=$1
    local force_flag=$2

    if [ -z "$agent_name" ]; then
        log_error "Usage: remove_agent <agent-name> [--force]"
        return 1
    fi

    local worktree_dir="euler-$agent_name"

    if [ ! -d "$worktree_dir" ]; then
        log_error "Worktree $worktree_dir does not exist"
        return 1
    fi

    # Safety check: refuse to remove dirty worktree unless --force
    cd "$worktree_dir"
    if [ "$force_flag" != "--force" ] && ! git diff --quiet; then
        log_error "Worktree has uncommitted changes. Use --force to override or commit changes first."
        echo "Uncommitted files:"
        git status --porcelain
        cd - > /dev/null
        return 1
    fi
    cd - > /dev/null

    log_info "Removing worktree for agent: $agent_name"

    # Remove worktree (with force if requested)
    if [ "$force_flag" = "--force" ]; then
        if ! git worktree remove --force "$worktree_dir"; then
            log_error "Failed to remove worktree $worktree_dir"
            return 1
        fi
    else
        if ! git worktree remove "$worktree_dir"; then
            log_error "Failed to remove worktree $worktree_dir"
            return 1
        fi
    fi

    # Note: Database cleanup removed - Euler_Problems has no database

    log_success "Agent $agent_name workspace removed"
}

# Note: Database management functions removed - Euler_Problems has no database requirements
# Note: Dependency synchronization removed - Euler_Problems uses per-problem build isolation

# Cleanup orphaned worktree references
cleanup_worktrees() {
    log_info "Cleaning up orphaned worktree references"
    git worktree prune
    log_success "Cleaned up orphaned worktree references"
}

# Show worktree status and health
# Environment: Set SHOW_DISK_USAGE=1 to include disk usage calculation
#              (adds ~3.5s for large worktrees, disabled by default to prevent shell prompt lag)
worktree_status() {
    echo "=== Git Worktree Status ==="
    git worktree list

    # Only calculate disk usage if explicitly requested (slow on large worktrees)
    # Accepts: 1, true, yes (case-insensitive)
    if [[ "${SHOW_DISK_USAGE,,}" =~ ^(1|true|yes)$ ]]; then
        echo -e "\n=== Disk Usage by Worktree ==="
        for worktree in euler-*; do
            if [ -d "$worktree" ]; then
                du -sh "$worktree" 2>/dev/null
            fi
        done
    fi

    echo -e "\n=== Health Check ==="
    local total_errors=0

    for worktree in euler-*; do
        if [ -d "$worktree" ]; then
            local agent_name=$(basename "$worktree" | sed 's/euler-//')
            echo "Checking $agent_name..."
            local agent_errors=0

            # Check required files
            for file in ".envrc" ".vscode/settings.json"; do
                if [[ ! -f "$worktree/$file" ]]; then
                    echo "  ❌ Missing required file: $file"
                    ((agent_errors++))
                fi
            done

            # Summary for this agent
            if [[ $agent_errors -eq 0 ]]; then
                echo "  ✅ Agent $agent_name: Healthy"
            else
                echo "  ❌ Agent $agent_name: $agent_errors errors found"
                ((total_errors += agent_errors))
            fi
            echo ""
        fi
    done

    # Overall health summary
    echo "=== Overall Health Summary ==="
    if [[ $total_errors -eq 0 ]]; then
        echo "✅ All agents are healthy"
    else
        echo "❌ Found $total_errors total issues across all agents"
    fi
}

# Show help and usage information
show_help() {
    cat <<'EOF'
Worktree Manager - Multi-Agent Worktree Management

USAGE:
    worktree_manager.sh <command> [options]
    worktree_manager.sh --help | -h

COMMANDS:
    list                     List all agent worktrees
    create <name> [branch]   Create new agent worktree with full setup
    remove <name> [--force]  Remove agent worktree (--force for dirty check)
    cleanup                  Clean up stale worktrees
    status                   Show worktree status and health check

    setup-direnv             Setup direnv hook for shell
    sync-envrc               Sync .envrc files across worktrees
    init-templates           Initialize worktree templates
    health | health-check    Run health check on all agents

    backup [path]            Backup .envrc file
    restore [path]           Restore .envrc from backup

ENVIRONMENT VARIABLES:
    SHOW_DISK_USAGE         Enable disk usage calculation in status output
                            Values: 1, true, yes (case-insensitive)
                            Warning: Adds ~3.5s for large worktrees (disabled by default)
                            
                            Examples:
                              SHOW_DISK_USAGE=1 worktree_manager.sh status
                              SHOW_DISK_USAGE=true worktree_manager.sh status

EXAMPLES:
    # Fast status (default - no disk usage)
    ./worktree_manager.sh status
    
    # Status with disk usage
    SHOW_DISK_USAGE=1 ./worktree_manager.sh status
    
    # Create new agent worktree
    ./worktree_manager.sh create claude
    
    # List all worktrees
    ./worktree_manager.sh list
    
    # Health check
    ./worktree_manager.sh health

For more information, see scripts/worktree_manager.sh documentation.
EOF
}

# Main command dispatcher
case "${1:-}" in
    "--help"|"-h"|"help") show_help ;;
    "list") list_agents ;;
    "create") create_agent "${2:-}" "${3:-}" ;;
    "remove") remove_agent "${2:-}" "${3:-}" ;;
    "cleanup") cleanup_worktrees ;;
    "status") worktree_status ;;

    # Environment management commands
    "setup-direnv") setup_direnv_hook ;;
    "sync-envrc") sync_envrc ;;
    "init-templates") init_templates ;;
    "health-check"|"health") health_check ;;

    # Backup and recovery commands
    "backup") backup_envrc "${2:-.}" ;;
    "restore") restore_envrc "${2:-.}" ;;

    *)
        echo "Usage: $0 {list|create|remove|cleanup|status|setup-direnv|sync-envrc|init-templates|health|backup|restore}"
        echo ""
        echo "Worktree Management:"
        echo "  list                     - List all agent worktrees"
        echo "  create <name> [branch]   - Create new agent worktree with full setup"
        echo "  remove <name> [--force]  - Remove agent worktree (--force to override dirty check)"
        echo "  cleanup                  - Clean up orphaned references"
        echo "  status                   - Show worktree status and health check"
        echo ""
        echo "Environment Management:"
        echo "  setup-direnv             - Configure direnv hooks for bash/zsh"
        echo "  sync-envrc               - Update all .envrc files from templates"
        echo "  init-templates           - Initialize template system (one-time setup)"
        echo "  health                   - Comprehensive environment health check"
        echo ""
        echo "Backup & Recovery:"
        echo "  backup [dir]             - Backup .envrc file (default: current dir)"
        echo "  restore [dir]            - Restore .envrc from most recent backup"
        echo ""
        echo "Examples:"
        echo "  $0 setup-direnv          # Configure shell integration"
        echo "  $0 sync-envrc            # Update all worktrees from templates"
        echo "  $0 health                # Check environment status"
        echo "  $0 create claude         # Create claude agent worktree"
        ;;
esac
