#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract current directory from JSON
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')

# Get current directory basename (equivalent to \W in PS1)
current_basename=$(basename "$current_dir")

# Get conda environment (if available)
conda_env_part=""
if [[ -n "${CONDA_DEFAULT_ENV}" && "${CONDA_DEFAULT_ENV}" != "base" ]]; then
    conda_env_part="${CONDA_DEFAULT_ENV}"
fi

# Get git status (equivalent to __git_ps1)
git_status_part=""
if git rev-parse --git-dir > /dev/null 2>&1; then
    # Check if we're in a git repository
    git_branch=$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
    if [[ -n "$git_branch" ]]; then
        # Check for dirty state (equivalent to GIT_PS1_SHOWDIRTYSTATE=1)
        dirty=""
        if [[ -n $(git status --porcelain 2>/dev/null) ]]; then
            dirty="*"
        fi
        
        # Check for untracked files (equivalent to GIT_PS1_SHOWUNTRACKEDFILES=1)
        untracked=""
        if [[ -n $(git ls-files --others --exclude-standard 2>/dev/null) ]]; then
            untracked="%"
        fi
        
        # Check upstream status (equivalent to GIT_PS1_SHOWUPSTREAM="auto")
        upstream=""
        if git rev-parse @{upstream} >/dev/null 2>&1; then
            ahead=$(git rev-list --count @{upstream}..HEAD 2>/dev/null)
            behind=$(git rev-list --count HEAD..@{upstream} 2>/dev/null)
            if [[ "$ahead" -gt 0 && "$behind" -gt 0 ]]; then
                upstream="<>"
            elif [[ "$ahead" -gt 0 ]]; then
                upstream=">"
            elif [[ "$behind" -gt 0 ]]; then
                upstream="<"
            fi
        fi
        
        git_status_part="${git_branch}${dirty}${untracked}${upstream}"
    fi
fi

# Build the status line with colors (using printf for ANSI codes)
# Note: The actual terminal will dim these colors automatically
status_line=""

# Add conda environment part (gray color)
if [[ -n "$conda_env_part" ]]; then
    status_line="${status_line}$(printf '\033[1;38;2;128;128;128m')${conda_env_part}$(printf '\033[0m')"
fi

# Add current directory (white color)
status_line="${status_line}$(printf '\033[1;38;2;255;255;255m')${current_basename}$(printf '\033[0m')"

# Add git status (cyan color)
if [[ -n "$git_status_part" ]]; then
    status_line="${status_line}$(printf '\033[1;38;2;0;255;255m')${git_status_part}$(printf '\033[0m')"
fi

# Add prompt symbol (red color) - removed trailing $ as per instructions
status_line="${status_line}$(printf '\033[1;38;2;255;85;85m')$(printf '\033[0m')"

echo "$status_line"