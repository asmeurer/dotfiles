#!/bin/bash
#
# Freeze the status line while this session's iTerm2 tab is in the background.
#
# Usage (from settings.json statusLine.command):
#     bash ~/.claude/statusline-freeze.sh <real statusline command...>
#
# stdin (the status line JSON) is read once here and piped through to the real
# command, whose stdout is passed back unchanged.
#
# Why this exists: statusLine.refreshInterval re-runs the status line on a
# timer so the clock stays live. That changed the rendered line every few
# seconds even on an idle session, Claude Code wrote the new frame to the PTY,
# and iTerm2 flagged the tab as having activity -- so every parked Claude tab
# looked like it had news.
#
# Claude Code's renderer diffs the frame cell by cell and emits stdout ops only
# for cells that actually changed, so replaying a BYTE-IDENTICAL line writes
# nothing at all and the indicator never fires. That is the whole trick: when
# the tab is not frontmost, replay the last line verbatim instead of rendering
# a new one. Refocusing restores a live line within one refreshInterval.
#
# This wraps the command rather than living inside it so the bash status line
# and the blackbox Python wrapper share one implementation -- and so only ONE
# layer ever freezes. Two nested layers would each run the focus check and
# fight over the same cache file.

set -uo pipefail

CACHE_DIR="$HOME/.claude/statusline-frozen"
ITERM_BUNDLE_ID="com.googlecode.iterm2"

# Which tab is frontmost is the same answer for every session, so all of them
# share one cached lookup. Without this, N Claude sessions each run their own
# AppleScript every refreshInterval and they all serialize on iTerm2's single
# Apple-event thread: measured at 7 sessions, wall time went 155ms -> 707ms and
# per-call latency ~155ms -> ~700ms, which risks making iTerm2 itself feel
# laggy. The TTL is deliberately under refreshInterval so it collapses the
# concurrent stampede without pinning a stale answer for a whole tick; the cost
# is that thawing a refocused tab can lag by up to TTL.
FRONT_CACHE="$CACHE_DIR/.frontmost"
FRONT_CACHE_TTL=2

ensure_cache_dir() {
    ( umask 077 && mkdir -p "$CACHE_DIR" ) 2>/dev/null || return 1
    [[ -d "$CACHE_DIR" && ! -L "$CACHE_DIR" && -O "$CACHE_DIR" ]] || return 1
    chmod 700 "$CACHE_DIR" 2>/dev/null || return 1
}

tighten_cache_file() {
    [[ -f "$1" && ! -L "$1" && -O "$1" ]] || return 1
    chmod 600 "$1" 2>/dev/null || return 1
}

valid_session_id() {
    [[ "$1" =~ ^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$ ]]
}

cache_target_is_safe() {
    local target="$1"
    local target_parent

    target_parent=$(dirname "$target") || return 1
    [[ "$target_parent" == "$CACHE_DIR" ]] || return 1
}

write_private_cache_file() {
    local target="$1"
    local base tmp

    ensure_cache_dir || return 0
    cache_target_is_safe "$target" || return 0
    base=$(basename "$target")
    tmp=$(mktemp "$CACHE_DIR/.${base}.XXXXXX") || return 0
    ( umask 077 && cat >"$tmp" ) 2>/dev/null &&
        chmod 600 "$tmp" 2>/dev/null &&
        mv -f "$tmp" "$target" 2>/dev/null &&
        chmod 600 "$target" 2>/dev/null
    rm -f "$tmp" 2>/dev/null
    return 0
}

# Whether this session's iTerm2 tab is the one being looked at.
#
# Returns true (render live) whenever focus cannot be established -- not
# iTerm2, no session id, a lookup that fails -- so an unknown state never
# freezes a stale line.
#
# Cheap check first: lsappinfo (~30ms) settles the common case, where some
# other app is frontmost and therefore EVERY iTerm2 tab is backgrounded. The
# ~200ms AppleScript that identifies the individual tab runs only when iTerm2
# itself is frontmost, which is the only time the answer depends on the tab.
tab_is_focused() {
    local uuid="${ITERM_SESSION_ID:-}"
    uuid="${uuid#*:}"                       # w1t12p0:UUID -> UUID
    [[ -z "$uuid" ]] && return 0

    local asn front_app front_session
    asn=$(lsappinfo front 2>/dev/null) || return 0
    [[ -z "$asn" ]] && return 0

    front_app=$(lsappinfo info -only bundleid "$asn" 2>/dev/null) || return 0
    # Another app entirely is frontmost, so no iTerm2 tab is focused. Note that
    # AppleScript's "current session of current window" answers even when iTerm2
    # is in the background, so this gate is load-bearing, not an optimization.
    [[ "$front_app" != *"$ITERM_BUNDLE_ID"* ]] && return 1

    front_session=$(read_front_cache)
    if [[ -z "$front_session" ]]; then
        front_session=$(osascript -e \
            'tell application "iTerm2" to return id of current session of current window' \
            2>/dev/null) || return 0
        [[ -z "$front_session" ]] && return 0
        write_front_cache "$front_session"
    fi
    [[ "$front_session" == "$uuid" ]]
}

# Frontmost session UUID if a sibling looked it up within the TTL, else empty.
read_front_cache() {
    local ts uuid age
    ensure_cache_dir || return 0
    tighten_cache_file "$FRONT_CACHE" || return 0
    [[ -r "$FRONT_CACHE" ]] || return 0
    read -r ts uuid <"$FRONT_CACHE" 2>/dev/null || return 0
    [[ -n "${ts:-}" && -n "${uuid:-}" ]] || return 0
    [[ "$ts" =~ ^[0-9]+$ ]] || return 0
    age=$(( $(date +%s) - ts ))
    # A negative age means a clock jump, not a fresh entry.
    (( age >= 0 && age < FRONT_CACHE_TTL )) && printf '%s' "$uuid"
}

# Publish via a temp file + mv so a concurrent reader never sees a half-written
# line. Racing writers are benign: they agree on the value.
write_front_cache() {
    printf '%s %s\n' "$(date +%s)" "$1" | write_private_cache_file "$FRONT_CACHE"
}

[[ $# -gt 0 ]] || { echo "usage: statusline-freeze.sh <command...>" >&2; exit 2; }

input=$(cat)

session_id=$(printf '%s' "$input" | jq -r '.session_id // empty' 2>/dev/null)
cache=
if [[ -n "$session_id" ]] && valid_session_id "$session_id"; then
    cache="$CACHE_DIR/$session_id.txt"
    cache_is_secure=false
    if ensure_cache_dir && tighten_cache_file "$cache"; then
        cache_is_secure=true
    fi
fi

if [[ -n "$cache" ]] && [[ "$cache_is_secure" == true ]] &&
    ! tab_is_focused && [[ -s "$cache" ]]; then
    cat "$cache"
    exit 0
fi

# Render live. Keep the real command's exit status and output even if the cache
# write fails -- a broken cache must never take down the status line.
output=$(printf '%s' "$input" | "$@")
status=$?

if [[ -n "$cache" ]]; then
    printf '%s' "$output" | write_private_cache_file "$cache"
fi

printf '%s' "$output"
exit $status
