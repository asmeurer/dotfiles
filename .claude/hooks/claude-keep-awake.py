#!/usr/bin/env python3
"""
Keep the Mac awake (and unlocked) only while a Claude Code session is working.

Why: git commits are signed with a Secretive (Secure Enclave) key, and that key
refuses to sign as soon as the account locks. Claude Code already prevents
idle *system* sleep while it is busy (it runs ``caffeinate -i`` itself), but
it does nothing about the *display*, which sleeps after a few idle minutes and
locks the screen immediately -- and from then on every commit fails with
"agent refused operation". So this hook holds a display-sleep assertion
(``caffeinate -d -i``) for each session, but only from the moment the session
starts working until it stops, ends, or blocks on the user. When no session is
working, nothing is held and the machine sleeps and locks as usual.

One caffeinate process per busy session, started with ``-w <claude pid>`` so
it dies on its own if the Claude Code process goes away without firing its
Stop/SessionEnd hooks (crash, kill -9, closed terminal). Each hold is tracked
in ~/.claude/keep-awake/<session_id>.pid.

Busy (hold):   UserPromptSubmit, PreToolUse, PostToolUse, Stop when
               stop_hook_active (Claude is being kept going by a Stop hook)
Idle (release): Stop, StopFailure, SessionEnd, Notification of type
               permission_prompt / idle_prompt / elicitation_dialog, and
               PreToolUse for AskUserQuestion / ExitPlanMode (all of these
               mean Claude is waiting for the user, so the screen may lock;
               the next tool use re-arms the hold).

Wire it up in ~/.claude/settings.json (hooks), one entry per event, no
matcher -- the script decides busy/idle itself from the payload:

    UserPromptSubmit, PreToolUse, PostToolUse, Stop, StopFailure,
    SessionEnd, Notification, SessionStart:
        python3 ~/.claude/hooks/claude-keep-awake.py <HookEventName>

Manual use:

    claude-keep-awake.py status       # list current holds
    claude-keep-awake.py release-all  # drop every hold now

Limitations: it cannot stop a lid close or a manual lock, and it keeps the
display on even on battery while sessions are busy (that is the point).
"""

import fcntl
import json
import os
import signal
import subprocess
import sys
from pathlib import Path

CAFFEINATE = "/usr/bin/caffeinate"
# -d: no idle display sleep (so no screen lock); -i: no idle system sleep.
CAFFEINATE_FLAGS = ["-d", "-i"]
STATE_DIR = Path.home() / ".claude" / "keep-awake"
LOCK_FILE = STATE_DIR / "lock"

BUSY_EVENTS = {"UserPromptSubmit", "PreToolUse", "PostToolUse"}
IDLE_EVENTS = {"Stop", "StopFailure", "SessionEnd"}
# Notification types that mean Claude is blocked waiting on the user.
WAITING_NOTIFICATIONS = {"permission_prompt", "idle_prompt", "elicitation_dialog"}
# Tools that block waiting on the user.
WAITING_TOOLS = {"AskUserQuestion", "ExitPlanMode"}


def ps(pid: int, field: str) -> str:
    """Return one ps(1) field for pid, or "" if the process does not exist."""
    result = subprocess.run(
        ["ps", "-o", f"{field}=", "-p", str(pid)], capture_output=True, text=True
    )
    return result.stdout.strip() if result.returncode == 0 else ""


def find_claude_pid() -> int:
    """Walk up from this hook to the Claude Code process that spawned it."""
    pid = os.getppid()
    while pid > 1:
        comm = ps(pid, "comm")
        if not comm:
            break
        if os.path.basename(comm) == "claude":
            return pid
        # Claude Code run through node: the script path names the package.
        if any("claude-code" in token for token in ps(pid, "args").split()):
            return pid
        pid = int(ps(pid, "ppid") or 0)
    return 0


def pidfile_for(session_id: str) -> Path:
    return STATE_DIR / f"{session_id}.pid"


def read_hold(path: Path):
    """Return (caffeinate_pid, claude_pid) from a pidfile, or None if stale."""
    try:
        caffeinate_pid, claude_pid = map(int, path.read_text().split())
    except (OSError, ValueError):
        return None
    if caffeinate_pid <= 0 or os.path.basename(ps(caffeinate_pid, "comm")) != "caffeinate":
        return None
    # Guard against pid reuse: the process must be *our* caffeinate.
    if f"-w {claude_pid}" not in ps(caffeinate_pid, "args"):
        return None
    return caffeinate_pid, claude_pid


def drop_stale_pidfiles() -> None:
    for path in STATE_DIR.glob("*.pid"):
        if read_hold(path) is None:
            path.unlink(missing_ok=True)


def hold(session_id: str) -> None:
    path = pidfile_for(session_id)
    if read_hold(path) is not None:
        return
    claude_pid = find_claude_pid()
    if not claude_pid:
        # Without a process to tie the hold to it could outlive the session
        # forever; better to not hold at all than to keep the screen on
        # indefinitely.
        sys.stderr.write("claude-keep-awake: could not find the claude process; not holding\n")
        path.unlink(missing_ok=True)
        return
    proc = subprocess.Popen(
        [CAFFEINATE, *CAFFEINATE_FLAGS, "-w", str(claude_pid)],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,  # detach, so Claude Code does not wait on it
    )
    path.write_text(f"{proc.pid} {claude_pid}\n")


def release(session_id: str) -> None:
    path = pidfile_for(session_id)
    held = read_hold(path)
    if held is not None:
        try:
            os.kill(held[0], signal.SIGTERM)
        except ProcessLookupError:
            pass
    path.unlink(missing_ok=True)


def status() -> None:
    drop_stale_pidfiles()
    holds = sorted(STATE_DIR.glob("*.pid"))
    if not holds:
        print("no sessions are holding the display awake")
        return
    for path in holds:
        caffeinate_pid, claude_pid = read_hold(path)
        print(f"{path.stem}: caffeinate pid {caffeinate_pid}, claude pid {claude_pid}")


def release_all() -> None:
    for path in STATE_DIR.glob("*.pid"):
        release(path.stem)


def decide(event: str, data: dict):
    """Return "busy", "idle", or None (no change) for a hook event."""
    if event == "PreToolUse":
        return "idle" if data.get("tool_name") in WAITING_TOOLS else "busy"
    if event in BUSY_EVENTS:
        return "busy"
    if event == "Stop":
        return "busy" if data.get("stop_hook_active") else "idle"
    if event in IDLE_EVENTS:
        return "idle"
    if event == "Notification":
        return "idle" if data.get("notification_type") in WAITING_NOTIFICATIONS else None
    return None


def main() -> None:
    if len(sys.argv) < 2:
        sys.exit(f"usage: {sys.argv[0]} <HookEventName> | status | release-all")
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    command = sys.argv[1]

    with open(LOCK_FILE, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if command == "status":
            status()
            return
        if command == "release-all":
            release_all()
            return

        data = json.load(sys.stdin) if not sys.stdin.isatty() else {}
        session_id = data.get("session_id", "")
        drop_stale_pidfiles()
        if not session_id:
            return
        action = decide(command, data)
        if action == "busy":
            hold(session_id)
        elif action == "idle":
            release(session_id)


if __name__ == "__main__":
    main()
