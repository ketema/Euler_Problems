#!/usr/bin/env bash
# validation/agent-names.sh
# Agent name validation for worktree management

# Colors for logging
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Agent name validation
readonly MAX_AGENT_NAME_LENGTH=40
readonly MIN_AGENT_NAME_LENGTH=2

validate_agent_name() {
    local agent_name=$1

    if [[ -z "$agent_name" ]]; then
        echo -e "${RED}Error: Agent name cannot be empty${NC}" >&2
        return 1
    fi

    if [[ ${#agent_name} -lt $MIN_AGENT_NAME_LENGTH ]]; then
        echo -e "${RED}Error: Agent name too short (minimum $MIN_AGENT_NAME_LENGTH characters)${NC}" >&2
        return 1
    fi

    if [[ ${#agent_name} -gt $MAX_AGENT_NAME_LENGTH ]]; then
        echo -e "${RED}Error: Agent name too long (maximum $MAX_AGENT_NAME_LENGTH characters)${NC}" >&2
        return 1
    fi

    # Security: Only allow alphanumeric, hyphen, underscore
    if [[ ! "$agent_name" =~ ^[a-zA-Z0-9_-]+$ ]]; then
        echo -e "${RED}Error: Invalid agent name format: $agent_name${NC}" >&2
        echo -e "${RED}Allowed characters: a-z, A-Z, 0-9, hyphen (-), underscore (_)${NC}" >&2
        return 1
    fi

    return 0
}
