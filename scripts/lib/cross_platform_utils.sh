#!/usr/bin/env bash

# Cross-Platform Shell Utilities
# Provides shell-agnostic functions for common operations
# Compatible with bash, zsh, and other POSIX shells

set -euo pipefail

# Get the directory of the current script in a cross-platform way
get_script_directory() {
    local script_path
    
    # Method 1: Try BASH_SOURCE (bash)
    if [[ -n "${BASH_SOURCE:-}" ]]; then
        script_path="${BASH_SOURCE[0]}"
    # Method 2: Try ZSH_ARGZERO (zsh)
    elif [[ -n "${ZSH_VERSION:-}" ]] && [[ -n "${(%):-%x}" ]]; then
        script_path="${(%):-%x}"
    # Method 3: Try $0 (fallback for other shells)
    else
        script_path="$0"
    fi
    
    # Resolve the directory
    local script_dir
    if command -v readlink >/dev/null 2>&1 && readlink -f / >/dev/null 2>&1; then
        # Linux/GNU readlink
        script_dir="$(cd "$(dirname "$(readlink -f "$script_path")")" && pwd)"
    elif command -v realpath >/dev/null 2>&1; then
        # macOS/BSD realpath
        script_dir="$(cd "$(dirname "$(realpath "$script_path")")" && pwd)"
    else
        # Fallback: basic resolution
        script_dir="$(cd "$(dirname "$script_path")" && pwd)"
    fi
    
    echo "$script_dir"
}

# Get the absolute path of the current script
get_script_path() {
    local script_path
    
    # Method 1: Try BASH_SOURCE (bash)
    if [[ -n "${BASH_SOURCE:-}" ]]; then
        script_path="${BASH_SOURCE[0]}"
    # Method 2: Try ZSH_ARGZERO (zsh)
    elif [[ -n "${ZSH_VERSION:-}" ]] && [[ -n "${(%):-%x}" ]]; then
        script_path="${(%):-%x}"
    # Method 3: Try $0 (fallback for other shells)
    else
        script_path="$0"
    fi
    
    # Resolve to absolute path
    if command -v readlink >/dev/null 2>&1 && readlink -f / >/dev/null 2>&1; then
        # Linux/GNU readlink
        readlink -f "$script_path"
    elif command -v realpath >/dev/null 2>&1; then
        # macOS/BSD realpath
        realpath "$script_path"
    else
        # Fallback: basic resolution
        if [[ "$script_path" = /* ]]; then
            echo "$script_path"
        else
            echo "$(pwd)/$script_path"
        fi
    fi
}

# Detect if script is being sourced or executed
is_script_sourced() {
    # Method 1: bash
    if [[ -n "${BASH_SOURCE:-}" ]]; then
        [[ "${BASH_SOURCE[0]}" != "${0}" ]]
    # Method 2: zsh
    elif [[ -n "${ZSH_VERSION:-}" ]]; then
        [[ "${(%):-%N}" != "${(%):-%x}" ]]
    # Method 3: fallback (less reliable)
    else
        # Check if $0 looks like a shell name or if we're in a sourced context
        case "$(basename "$0")" in
            bash|zsh|sh|dash|ksh) return 0 ;;
            *)
                # Additional check: if $0 contains the script name, it's likely executed
                if [[ "$0" == *"$(basename "${BASH_SOURCE[0]:-$0}")"* ]]; then
                    return 1
                else
                    return 0
                fi
                ;;
        esac
    fi
}

# Get the name of the current shell
get_shell_name() {
    if [[ -n "${BASH_VERSION:-}" ]]; then
        echo "bash"
    elif [[ -n "${ZSH_VERSION:-}" ]]; then
        echo "zsh"
    elif [[ -n "${KSH_VERSION:-}" ]]; then
        echo "ksh"
    else
        basename "${SHELL:-sh}"
    fi
}

# Cross-platform array handling
# Usage: cross_platform_array_contains "element" "${array[@]}"
cross_platform_array_contains() {
    local element="$1"
    shift
    local item
    for item in "$@"; do
        [[ "$item" == "$element" ]] && return 0
    done
    return 1
}

# Cross-platform string manipulation
# Usage: cross_platform_lowercase "STRING"
cross_platform_lowercase() {
    local string="$1"
    if command -v tr >/dev/null 2>&1; then
        echo "$string" | tr '[:upper:]' '[:lower:]'
    else
        # Fallback for systems without tr
        echo "${string,,}" 2>/dev/null || echo "$string" | awk '{print tolower($0)}'
    fi
}

# Cross-platform string manipulation
# Usage: cross_platform_uppercase "string"
cross_platform_uppercase() {
    local string="$1"
    if command -v tr >/dev/null 2>&1; then
        echo "$string" | tr '[:lower:]' '[:upper:]'
    else
        # Fallback for systems without tr
        echo "${string^^}" 2>/dev/null || echo "$string" | awk '{print toupper($0)}'
    fi
}

# Test if running on macOS
is_macos() {
    [[ "$(uname -s)" == "Darwin" ]]
}

# Test if running on Linux
is_linux() {
    [[ "$(uname -s)" == "Linux" ]]
}

# Get OS name in lowercase
get_os_name() {
    cross_platform_lowercase "$(uname -s)"
}

# Cross-platform process checking
# Usage: is_process_running "process_name"
is_process_running() {
    local process_name="$1"
    if command -v pgrep >/dev/null 2>&1; then
        pgrep -f "$process_name" >/dev/null 2>&1
    elif command -v ps >/dev/null 2>&1; then
        ps aux | grep -v grep | grep -q "$process_name"
    else
        return 1
    fi
}

# Cross-platform file modification time
# Returns seconds since epoch
get_file_mtime() {
    local file="$1"
    if is_macos; then
        stat -f "%m" "$file" 2>/dev/null
    else
        stat -c "%Y" "$file" 2>/dev/null
    fi
}

# Cross-platform file size
get_file_size() {
    local file="$1"
    if is_macos; then
        stat -f "%z" "$file" 2>/dev/null
    else
        stat -c "%s" "$file" 2>/dev/null
    fi
}

# Validate cross-platform utilities
validate_cross_platform_utils() {
    echo "Cross-Platform Utilities Validation"
    echo "===================================="
    
    echo "Shell: $(get_shell_name)"
    echo "OS: $(get_os_name)"
    echo "Script directory: $(get_script_directory)"
    echo "Script path: $(get_script_path)"
    echo "Is sourced: $(is_script_sourced && echo "yes" || echo "no")"
    
    # Test string functions
    local test_string="Hello World"
    echo "Lowercase test: $(cross_platform_lowercase "$test_string")"
    echo "Uppercase test: $(cross_platform_uppercase "$test_string")"
    
    # Test array function
    local test_array=("apple" "banana" "cherry")
    if cross_platform_array_contains "banana" "${test_array[@]}"; then
        echo "Array contains test: ✅ PASS"
    else
        echo "Array contains test: ❌ FAIL"
    fi
    
    echo "Validation complete."
}

# Command-line interface
if ! is_script_sourced; then
    case "${1:-}" in
        --help|-h)
            echo "Cross-Platform Shell Utilities"
            echo ""
            echo "Usage: source cross_platform_utils.sh"
            echo "       cross_platform_utils.sh --validate"
            echo ""
            echo "Functions available after sourcing:"
            echo "  get_script_directory    - Get directory of current script"
            echo "  get_script_path         - Get absolute path of current script"
            echo "  is_script_sourced       - Check if script is being sourced"
            echo "  get_shell_name          - Get name of current shell"
            echo "  cross_platform_*        - Various cross-platform utilities"
            echo "  is_macos/is_linux       - OS detection"
            echo "  validate_cross_platform_utils - Run validation tests"
            ;;
        --validate)
            validate_cross_platform_utils
            ;;
        *)
            echo "Usage: source cross_platform_utils.sh"
            echo "       cross_platform_utils.sh --help"
            echo "       cross_platform_utils.sh --validate"
            ;;
    esac
fi
