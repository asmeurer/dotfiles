#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract current directory from JSON
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')

# Extract context window information
used_percentage=$(echo "$input" | jq -r '.context_window.used_percentage // 0')
tokens_total=$(echo "$input" | jq -r '.context_window.context_window_size // 0')

# Extract model display name, id, and reasoning effort level
model_name=$(echo "$input" | jq -r '.model.display_name // empty')
model_id=$(echo "$input" | jq -r '.model.id // empty')
effort_level=$(echo "$input" | jq -r '.effort.level // empty')

# Detect whether Fable is the active model. Fable draws from a separate
# per-model weekly usage bucket (not exposed in the statusline JSON's
# rate_limits) — see the Fable rate-limit block below.
model_is_fable=""
if [[ "$(echo "$model_name" | tr '[:upper:]' '[:lower:]')" == *"fable"* || "$model_id" == *"fable"* ]]; then
    model_is_fable="1"
fi

# Extract rate limits (Claude.ai subscription usage limits)
five_hour_pct=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
five_hour_resets=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty')
seven_day_pct=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
seven_day_resets=$(echo "$input" | jq -r '.rate_limits.seven_day.resets_at // empty')

# Format token counts (convert to k format if > 1000)
format_tokens() {
    local num=$1
    if [[ $num -ge 1000 ]]; then
        echo "$((num / 1000))k"
    else
        echo "$num"
    fi
}

# Format a duration in minutes as compact human string
format_mins() {
    local total_mins=$1
    local d=$(( total_mins / 1440 ))
    local h=$(( (total_mins % 1440) / 60 ))
    local m=$(( total_mins % 60 ))
    if [[ $d -gt 0 ]]; then
        echo "${d}d ${h}h"
    elif [[ $h -gt 0 ]]; then
        echo "${h}h ${m}m"
    else
        echo "${m}m"
    fi
}

# Format seconds-until-reset as compact human duration
format_reset() {
    local resets_at=$1
    local now
    now=$(date +%s)
    local secs_left=$(( resets_at - now ))
    if [[ $secs_left -le 0 ]]; then
        echo "now"
        return
    fi
    format_mins $(( secs_left / 60 ))
}

# Compute projected minutes until budget empty at current burn rate.
# Prints an integer, or empty string if projection isn't meaningful.
# Args: used_pct  resets_at  window_mins
project_empty_mins() {
    local used_pct=$1
    local resets_at=$2
    local window_mins=$3
    local now
    now=$(date +%s)
    local mins_until_reset=$(( (resets_at - now) / 60 ))
    local mins_elapsed=$(( window_mins - mins_until_reset ))
    # Need meaningful elapsed time and at least some usage
    [[ $mins_elapsed -le 0 ]] && return
    local out_mins
    out_mins=$(echo "scale=0; r = (100 - $used_pct) * $mins_elapsed / $used_pct; if (r < 0) { 0 } else { r / 1 }" | bc -l 2>/dev/null) || return
    [[ -z "$out_mins" || "$out_mins" == "0" ]] && return
    # Only meaningful if it runs out before reset
    if [[ $out_mins -lt $mins_until_reset ]]; then
        echo "$out_mins"
    fi
}

# Convert an ISO-8601 UTC timestamp to a Unix epoch. Prints nothing if the
# string cannot be parsed, so callers fall back to their no-clock rendering.
#
# claude-swap reports scoped-window reset times as ISO strings
# ("2026-08-20T14:00:00.061597+00:00") while the status line JSON's rate_limits
# already carry epochs. Normalizing here lets the Fable row reuse the same
# projection math as 5h/7d instead of a second, drifting implementation.
#
# The fractional seconds and zone offset are trimmed rather than parsed: every
# timestamp on this path is UTC, and neither date implementation accepts "%S.%f"
# portably. BSD date (macOS) needs -j -f; GNU date (Linux) needs -d.
iso_to_epoch() {
    local iso=$1
    [[ -n "$iso" ]] || return
    local stamp=${iso%%.*}      # drop fractional seconds
    stamp=${stamp%%+*}          # drop "+00:00" offset
    stamp=${stamp%Z}            # drop trailing "Z"
    date -j -u -f "%Y-%m-%dT%H:%M:%S" "$stamp" +%s 2>/dev/null \
        || date -u -d "${stamp}Z" +%s 2>/dev/null
}

auto_swap_state_json=""
read_auto_swap_state() {
    local state_path="$HOME/.claude/auto-swap-state.json"
    if [[ -z "$auto_swap_state_json" && -r "$state_path" ]]; then
        auto_swap_state_json=$(jq -c . "$state_path" 2>/dev/null || true)
    fi
}

auto_swap_fable_recently_active() {
    read_auto_swap_state
    [[ -n "$auto_swap_state_json" ]] || return 1
    local now last_seen last_walled
    now=$(date +%s)
    last_seen=$(echo "$auto_swap_state_json" | jq -r '.fable_last_seen_ts // empty' 2>/dev/null)
    last_walled=$(echo "$auto_swap_state_json" | jq -r '.fable_walled_ts // empty' 2>/dev/null)
    if [[ -n "$last_seen" ]] && [[ "$(echo "$now - $last_seen < 900" | bc -l 2>/dev/null)" == "1" ]]; then
        return 0
    fi
    if [[ -n "$last_walled" ]] && [[ "$(echo "$now - $last_walled < 600" | bc -l 2>/dev/null)" == "1" ]]; then
        return 0
    fi
    return 1
}

# Pick ANSI color based on percentage remaining (inverted: high remaining = green)
rate_limit_color() {
    local pct_left=$1
    pct_left=$(printf '%.0f' "$pct_left")
    if [[ $pct_left -le 10 ]]; then
        printf '\033[1;38;2;255;85;85m'    # red: almost out
    elif [[ $pct_left -le 30 ]]; then
        printf '\033[1;38;2;255;185;0m'    # gold: getting low
    else
        printf '\033[1;38;2;100;220;120m'  # green: plenty left
    fi
}

tokens_total_formatted=$(format_tokens "$tokens_total")

# Use the pre-calculated percentage
tokens_percentage=$(printf '%.0f' "$used_percentage")

# Get current directory basename (equivalent to \W in PS1)
current_basename=$(basename "$current_dir")

# Get conda environment (if available)
conda_env_part=""
if [[ -n "${CONDA_DEFAULT_ENV}" && "${CONDA_DEFAULT_ENV}" != "base" ]]; then
    conda_env_part="${CONDA_DEFAULT_ENV}"
fi

# Get account email from ~/.claude.json
account_email=""
if [[ -f "$HOME/.claude.json" ]]; then
    account_email=$(jq -r '.oauthAccount.emailAddress // empty' "$HOME/.claude.json" 2>/dev/null)
fi

# Run git from the session's directory. Everything else on this line is
# derived from the status line JSON, so without this the branch state could
# describe a different repository than the basename sitting next to it
# whenever the status line subprocess's inherited cwd differs from
# workspace.current_dir.
if [[ -d "$current_dir" ]]; then
    cd "$current_dir" || true
fi

# Get git status by delegating to git's own __git_ps1 -- the very function the
# bash prompt uses (.profile sources $GIT_PROMPT_FILE) -- so the status line
# and the shell prompt cannot drift apart.
#
# A hand-rolled "equivalent to __git_ps1" used to live here and had silently
# diverged in five ways: it drove the "*" off `git status --porcelain`, which
# counts untracked files, so repos with nothing uncommitted still showed "*";
# it collapsed staged changes into "*" instead of "+"; it scoped the untracked
# check to the cwd rather than the whole repo; it omitted the "=" that marks a
# branch level with its upstream; and it knew nothing of rebase/merge/bisect
# state, unborn branches, or the parenthesized detached-HEAD format.
#
# Mirror .profile's prompt configuration rather than inheriting it: Claude Code
# need not have been launched from an interactive bash, and an unset
# GIT_PS1_SHOWDIRTYSTATE would silently drop the markers instead of failing.
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_STATESEPARATOR=''

git_prompt_file="${GIT_PROMPT_FILE:-$HOME/Documents/git/contrib/completion/git-prompt.sh}"
if [[ -r "$git_prompt_file" ]]; then
    source "$git_prompt_file"
fi

git_status_part=""
if declare -F __git_ps1 >/dev/null; then
    # Prints nothing at all when outside a work tree.
    git_status_part=$(__git_ps1 "%s")
else
    # git-prompt.sh arrives with the git clone that gitclones.sh makes. Until
    # that has run, show the branch name alone rather than reintroducing a
    # second, drift-prone copy of the decoration logic.
    git_status_part=$(git symbolic-ref --short HEAD 2>/dev/null \
        || git rev-parse --short HEAD 2>/dev/null)
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

# Add wall clock (yellow). Kept current by statusLine.refreshInterval; a
# backgrounded tab replays a frozen line via statusline-freeze.sh so the
# ticking does not trip iTerm2's tab activity indicator.
status_line="${status_line}  $(printf '\033[33m')⏱ $(date '+%-I:%M %p')$(printf '\033[0m')"

echo "$status_line"

# Build second line: context info, model + effort, account email, rate limits
second_line=""
if [[ $tokens_total -gt 0 ]]; then
    context_info="[${tokens_percentage}% of ${tokens_total_formatted}]"
    second_line="$(printf '\033[1;38;2;255;200;100m')${context_info}$(printf '\033[0m')"
fi
if [[ -n "$model_name" ]]; then
    # Pick color by model family
    model_name_lower=$(echo "$model_name" | tr '[:upper:]' '[:lower:]')
    if [[ "$model_name_lower" == *"fable"* ]]; then
        model_color=$(printf '\033[1;38;2;200;130;255m')   # purple
    elif [[ "$model_name_lower" == *"opus"* ]]; then
        model_color=$(printf '\033[1;38;2;255;185;0m')     # gold
    elif [[ "$model_name_lower" == *"haiku"* ]]; then
        model_color=$(printf '\033[1;38;2;100;220;120m')   # green
    else
        model_color=$(printf '\033[1;38;2;150;200;255m')   # blue (sonnet / default)
    fi

    # Pick color by effort level
    effort_color=""
    if [[ -n "$effort_level" ]]; then
        effort_level_lower=$(echo "$effort_level" | tr '[:upper:]' '[:lower:]')
        if [[ "$effort_level_lower" == "high" ]]; then
            effort_color="$model_color"                        # default — match model
        elif [[ "$effort_level_lower" == *"turbo"* || "$effort_level_lower" == *"fast"* ]]; then
            effort_color=$(printf '\033[1;38;2;255;140;0m')    # orange
        elif [[ "$effort_level_lower" == *"extend"* ]]; then
            effort_color=$(printf '\033[1;38;2;80;255;80m')    # bright green
        else
            effort_color=$(printf '\033[1;38;2;255;255;100m')  # yellow (unexpected)
        fi
    fi

    if [[ -n "$second_line" ]]; then
        second_line="${second_line} "
    fi
    second_line="${second_line}${model_color}${model_name}$(printf '\033[0m')"
    if [[ -n "$effort_level" ]]; then
        second_line="${second_line}${effort_color} (${effort_level})$(printf '\033[0m')"
    fi
fi
if [[ -n "$account_email" ]]; then
    if [[ -n "$second_line" ]]; then
        second_line="${second_line} "
    fi
    second_line="${second_line}$(printf '\033[1;38;2;128;128;128m')${account_email}$(printf '\033[0m')"
fi

if [[ -n "$second_line" ]]; then
    echo "$second_line"
fi

# Third line: rate limits (5-hour and 7-day subscription usage limits)
dim=$(printf '\033[38;2;160;160;160m')
danger_col=$(printf '\033[1;38;2;255;85;85m')
ansi_reset=$(printf '\033[0m')
rate_line=""

render_rate_limit() {
    local label=$1        # "5h" or "7d"
    local used_pct=$2
    local resets_at=$3
    local window_mins=$4
    local pct_left
    pct_left=$(printf '%.0f' "$(echo "100 - $used_pct" | bc)")
    local col
    col=$(rate_limit_color "$pct_left")

    local suffix=""
    if [[ -n "$resets_at" ]]; then
        local empty_mins
        empty_mins=$(project_empty_mins "$used_pct" "$resets_at" "$window_mins")
        if [[ -n "$empty_mins" ]]; then
            suffix="${danger_col} empty ~$(format_mins "$empty_mins")${ansi_reset}${dim} | reset $(format_reset "$resets_at")${ansi_reset}"
        else
            suffix="${dim} → $(format_reset "$resets_at")${ansi_reset}"
        fi
    fi
    printf '%s' "${dim}${label} ${ansi_reset}${col}${pct_left}%${ansi_reset}${suffix}"
}

# Render Fable's per-model weekly usage, sourced from claude-swap (which reads
# /api/oauth/usage) since the statusline JSON does not carry per-model buckets.
#
# Delegates to render_rate_limit so the Fable row is formatted, colored, and
# projected identically to 5h/7d — including the "empty ~Xh Ym" estimate. It
# only needs its own wrapper because its reset time arrives as an ISO string
# rather than an epoch; ``countdown`` is claude-swap's pre-formatted reset text,
# used only when that timestamp is missing or unparseable.
render_fable_limit() {
    local used_pct=$1
    local countdown=$2
    local resets_at_iso=$3
    local resets_at
    resets_at=$(iso_to_epoch "$resets_at_iso")
    if [[ -n "$resets_at" ]]; then
        # Fable's bucket rides the same weekly window as 7d (identical resetsAt).
        render_rate_limit "Fable" "$used_pct" "$resets_at" 10080
        return
    fi
    local pct_left
    pct_left=$(printf '%.0f' "$(echo "100 - $used_pct" | bc)")
    local col
    col=$(rate_limit_color "$pct_left")
    local suffix=""
    if [[ -n "$countdown" ]]; then
        suffix="${dim} → ${countdown}${ansi_reset}"
    fi
    printf '%s' "${dim}Fable ${ansi_reset}${col}${pct_left}%${ansi_reset}${suffix}"
}

if [[ -n "$five_hour_pct" ]]; then
    rate_line="${rate_line}$(render_rate_limit "5h" "$five_hour_pct" "$five_hour_resets" 300)"
fi
if [[ -n "$seven_day_pct" && -n "$seven_day_resets" ]]; then
    seven_day_empty_mins=$(project_empty_mins "$seven_day_pct" "$seven_day_resets" 10080)
    if [[ -n "$seven_day_empty_mins" ]]; then
        if [[ -n "$rate_line" ]]; then rate_line="${rate_line}  ${dim}|${ansi_reset}  "; fi
        rate_line="${rate_line}$(render_rate_limit "7d" "$seven_day_pct" "$seven_day_resets" 10080)"
    fi
fi
# Fetch claude-swap's account/usage JSON once, only when something below needs
# it: the Fable per-model bucket (Fable is the active model) or the
# alternate-account note (the auto-swap fallback account is active). Omitted
# silently on any failure so the rate line always renders.
alt_primary_email="asmeurer@gmail.com"      # auto-swap hook's primary account
alt_fallback_email="aaronmeurer@gmail.com"  # auto-swap hook's fallback account
swap_json=""
if [[ -n "$model_is_fable" || "$account_email" == "$alt_fallback_email" ]]; then
    swap_bin="$HOME/.local/bin/claude-swap"
    [[ -x "$swap_bin" ]] || swap_bin=$(command -v claude-swap 2>/dev/null)
    if [[ -n "$swap_bin" ]]; then
        swap_timeout=()
        if command -v timeout >/dev/null 2>&1; then
            swap_timeout=(timeout 4)
        elif command -v gtimeout >/dev/null 2>&1; then
            swap_timeout=(gtimeout 4)
        fi
        if [[ ${#swap_timeout[@]} -gt 0 ]]; then
            swap_json=$("${swap_timeout[@]}" "$swap_bin" --list --json 2>/dev/null)
        fi
    fi
fi

# Fable per-model weekly usage — only when Fable is the active model. Read from
# claude-swap's --json output (the active account's "Fable" scoped entry).
if [[ -n "$model_is_fable" && -n "$swap_json" ]]; then
    fable_scoped='.accounts[]? | select(.active) | .usage.scoped[]? | select(.name=="Fable")'
    fable_pct=$(echo "$swap_json" | jq -r "$fable_scoped | .pct // empty" 2>/dev/null | head -1)
    fable_reset=$(echo "$swap_json" | jq -r "$fable_scoped | .countdown // empty" 2>/dev/null | head -1)
    fable_resets_at=$(echo "$swap_json" | jq -r "$fable_scoped | .resetsAt // empty" 2>/dev/null | head -1)
    if [[ -n "$fable_pct" ]]; then
        if [[ -n "$rate_line" ]]; then rate_line="${rate_line}  ${dim}|${ansi_reset}  "; fi
        rate_line="${rate_line}$(render_fable_limit "$fable_pct" "$fable_reset" "$fable_resets_at")"
    fi
fi

# Alternate-account note: while on the auto-swap fallback account, show when
# the primary has 5-hour headroom again -- the cue that a switch back is
# possible. Mirrors auto-swap-on-low-usage.py's gating: primary usageStatus
# "ok", 5-hour window meaningfully recovered (>=10% free), weekly cap not
# walled (>5% free; missing weekly data counts as ok), and -- when the hook's
# shared state says Fable is active in any session -- the primary's Fable weekly
# bucket not spent (>5% free, or no pending Fable reset when live usage is
# unavailable), since the hook deliberately holds on the fallback in that case.
if [[ "$account_email" == "$alt_fallback_email" && -n "$swap_json" ]]; then
    alt_primary='.accounts[]? | select(.email=="'"$alt_primary_email"'")'
    alt_status=$(echo "$swap_json" | jq -r "$alt_primary | .usageStatus // empty" 2>/dev/null | head -1)
    alt_5h_pct=$(echo "$swap_json" | jq -r "$alt_primary | .usage.fiveHour.pct // empty" 2>/dev/null | head -1)
    alt_7d_pct=$(echo "$swap_json" | jq -r "$alt_primary | .usage.sevenDay.pct // empty" 2>/dev/null | head -1)
    alt_fable_held=""
    if auto_swap_fable_recently_active; then
        alt_fable_pct=$(echo "$swap_json" | jq -r "$alt_primary | .usage.scoped[]? | select(.name==\"Fable\") | .pct // empty" 2>/dev/null | head -1)
        if [[ -n "$alt_fable_pct" ]] && [[ "$(echo "100 - $alt_fable_pct <= 5" | bc -l 2>/dev/null)" == "1" ]]; then
            alt_fable_held="1"
        elif [[ -z "$alt_fable_pct" ]]; then
            read_auto_swap_state
            pending_fable_reset=$(echo "$auto_swap_state_json" | jq -r '.pending_fable_reset_at // empty' 2>/dev/null)
            pending_fable_reset_epoch=$(iso_to_epoch "$pending_fable_reset")
            if [[ -n "$pending_fable_reset_epoch" && "$pending_fable_reset_epoch" -gt "$(date +%s)" ]]; then
                alt_fable_held="1"
            fi
        fi
    fi
    if [[ -n "$alt_fable_held" ]]; then
        alt_status=""   # hook is holding here for Fable — don't advertise a return
    fi
    if [[ "$alt_status" == "ok" && -n "$alt_5h_pct" ]]; then
        alt_5h_free_raw=$(echo "100 - $alt_5h_pct" | bc -l 2>/dev/null)
        alt_5h_recovered=$(echo "$alt_5h_free_raw >= 10" | bc -l 2>/dev/null)
        alt_5h_free=$(printf '%.0f' "$alt_5h_free_raw")
        alt_weekly_ok=1
        if [[ -n "$alt_7d_pct" ]]; then
            alt_7d_free_raw=$(echo "100 - $alt_7d_pct" | bc -l 2>/dev/null)
            alt_weekly_ok=$(echo "$alt_7d_free_raw > 5" | bc -l 2>/dev/null)
        fi
        if [[ "$alt_weekly_ok" == "1" && "$alt_5h_recovered" == "1" ]]; then
            alt_green=$(printf '\033[1;38;2;100;220;120m')
            if [[ -n "$rate_line" ]]; then rate_line="${rate_line}  ${dim}|${ansi_reset}  "; fi
            rate_line="${rate_line}${alt_green}↩ ${alt_primary_email%%@*} ${alt_5h_free}% free${ansi_reset}"
        fi
    fi
fi

if [[ -n "$rate_line" ]]; then
    echo "$rate_line"
fi
