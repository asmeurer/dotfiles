#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = ["iterm2"]
# ///
"""
Let iTerm2 quit (and macOS restart for updates) while Claude Code sessions are
idle, and resume those sessions automatically when iTerm2 comes back.

Two problems, two halves:

1. Idle sessions block quitting. The profile's "Prompt Before Closing: if
   there are jobs besides ..." setting walks the *whole* process tree under
   the shell, so a running ``claude`` (plus hunspell, the statusline's git,
   MCP servers, ...) always counts as a job, and the "Quit iTerm2?" alert
   cancels a macOS update restart. Instead of maintaining a Jobs to Ignore
   list, this hook sets "Prompt Before Closing 2" on the *session* (a
   session-local profile override through the iTerm2 Python API): Never while
   Claude is idle, and back to the shared profile's own value while it is
   working. A working session still blocks quit exactly as before.

   Busy:  UserPromptSubmit, PreToolUse, PostToolUse, Stop when
          stop_hook_active or when background tasks are still running.
   Idle:  SessionStart, Stop, StopFailure, Notification of type idle_prompt
          (also covers interrupted turns, which fire no Stop), and PreToolUse
          for AskUserQuestion / ExitPlanMode.
   Permission prompts leave the session busy on purpose: once approved, the
   tool runs with no further hook event until it finishes, so treating the
   prompt as idle would leave e.g. an approved test run unprotected.

   The API calls happen in a detached worker (``apply``) so hooks stay fast.
   The hook writes the desired state into the pane's record; the worker
   compares it with the last state it applied and only talks to iTerm2 on a
   change. The iterm2 module authenticates with an AppleScript cookie request
   on every connection (cookies are single use), and an Apple event that
   arrives after iTerm2 has exited relaunches it mid-quit and wipes its saved
   windows -- so nothing here talks to iTerm2 when a session ends by hangup,
   and the worker checks that iTerm2 is running first.

2. Quitting kills the sessions. Each iTerm2 pane gets a record in
   ~/.claude/session-restore/<pane GUID>.json naming the Claude session
   running in it. iTerm2 restores panes with their old GUIDs, so at the end
   of ~/.profile a restored pane runs ``shell-resume``, which finds the
   record for its own GUID whose claude process is gone and prints the
   ``cd ... && claude --resume ...`` command for the shell to run. A record
   is removed when Claude exits deliberately (/exit, ctrl-d, ...), so only
   sessions that were killed come back. A pane closed with cmd-w leaves a
   record too, but its GUID never returns; ``list`` / ``open`` reach those
   (or every session, if iTerm2 did not restore its windows).

Wire it up in ~/.claude/settings.json (hooks), no matcher, for SessionStart,
UserPromptSubmit, PreToolUse, PostToolUse, Stop, StopFailure, Notification,
SessionEnd:

    python3 ~/.claude/hooks/claude-session-restore.py hook

Manual use:

    claude-session-restore.py list              # sessions that can be resumed
    claude-session-restore.py open --all        # resume them in new tabs
    claude-session-restore.py open <session id> ...

The hook path only needs the standard library. ``apply`` and ``open`` re-run
this file under ``uv run --script`` to get the iterm2 module. The worker's
output, including tracebacks, goes to ~/.claude/session-restore/worker.log.
"""

import fcntl
import json
import os
import shlex
import subprocess
import sys
import time
from pathlib import Path

STATE_DIR = Path.home() / ".claude" / "session-restore"
WORKER_LOG = STATE_DIR / "worker.log"
UV = "/opt/homebrew/bin/uv"

PROMPT_KEY = "Prompt Before Closing 2"
PROMPT_NEVER = 0

# Tools that block waiting on the user. PostToolUse fires when they return.
WAITING_TOOLS = {"AskUserQuestion", "ExitPlanMode"}
# SessionEnd reasons where the user ended the session on purpose. Anything
# else ("other") is a hangup or kill, i.e. the session should come back.
DELIBERATE_END_REASONS = {
    "clear",
    "resume",
    "logout",
    "prompt_input_exit",
    "bypass_permissions_disabled",
}
# /clear and /resume keep the same claude process; a SessionStart for the new
# session follows and takes over the pane's record.
SAME_PROCESS_END_REASONS = {"clear", "resume"}
# Records for dead sessions older than this are dropped.
MAX_ORPHAN_AGE = 14 * 24 * 3600


# ---------------------------------------------------------------- records


def pane_guid() -> str:
    """The GUID part of $ITERM_SESSION_ID (w0t1p0:GUID), or ""."""
    return os.environ.get("ITERM_SESSION_ID", "").partition(":")[2]


def record_path(guid: str) -> Path:
    return STATE_DIR / f"{guid}.json"


def applied_path(guid: str) -> Path:
    return STATE_DIR / f"{guid}.applied"


class PaneLock:
    """Exclusive lock on one pane's record (and applied state)."""

    def __init__(self, guid: str, name: str = "lock"):
        self.path = STATE_DIR / f"{guid}.{name}"

    def __enter__(self):
        self.file = open(self.path, "w")
        fcntl.flock(self.file, fcntl.LOCK_EX)
        return self

    def __exit__(self, *exc):
        self.file.close()


def read_json(path: Path):
    try:
        return json.loads(path.read_text())
    except FileNotFoundError:
        return None


def write_json(path: Path, data) -> None:
    tmp = path.with_name(path.name + ".tmp")
    tmp.write_text(json.dumps(data, indent=2) + "\n")
    os.replace(tmp, path)


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


def claude_alive(record: dict) -> bool:
    """Whether the record's claude process is still running.

    The start time guards against pid reuse, which is likely after a reboot.
    """
    pid = record.get("claude_pid") or 0
    lstart = ps(pid, "lstart") if pid > 0 else ""
    return bool(lstart) and lstart == record.get("claude_lstart")


def iterm2_running() -> bool:
    result = subprocess.run(["pgrep", "-x", "iTerm2"], capture_output=True)
    return result.returncode == 0


def resume_command(record: dict) -> str:
    return (
        f"cd -- {shlex.quote(record['cwd'])} && "
        f"claude --resume {shlex.quote(record['session_id'])}"
    )


def orphans() -> list:
    """Records whose claude process is gone, newest first. Prunes old ones."""
    result = []
    for path in STATE_DIR.glob("*.json"):
        guid = path.stem
        with PaneLock(guid):
            record = read_json(path)
            if record is None or claude_alive(record):
                continue
            if time.time() - record.get("updated", 0) > MAX_ORPHAN_AGE:
                path.unlink()
                applied_path(guid).unlink(missing_ok=True)
                continue
        result.append((guid, record))
    # Lock files of panes that no longer have a record.
    for path in [*STATE_DIR.glob("*.lock"), *STATE_DIR.glob("*.worker-lock")]:
        guid = path.name.partition(".")[0]
        if not record_path(guid).exists() and time.time() - path.stat().st_mtime > 24 * 3600:
            path.unlink()
    result.sort(key=lambda item: item[1].get("updated", 0), reverse=True)
    return result


# ------------------------------------------------------------------- hook


def desired_state(event: str, data: dict):
    """Return "busy", "idle", or None (no change) for a hook event."""
    if event == "SessionStart":
        return "idle"
    if event == "UserPromptSubmit" or event == "PostToolUse":
        return "busy"
    if event == "PreToolUse":
        return "idle" if data.get("tool_name") in WAITING_TOOLS else "busy"
    if event == "Stop":
        if data.get("stop_hook_active"):
            return "busy"
        # Background shells/agents keep working after the turn ends. Older
        # Claude Code versions don't send this field.
        tasks = data.get("background_tasks") or []
        if any(task.get("status", "running") in ("running", "pending") for task in tasks):
            return "busy"
        return "idle"
    if event == "StopFailure":
        return "idle"
    if event == "Notification":
        return "idle" if data.get("notification_type") == "idle_prompt" else None
    return None


def spawn_worker(guid: str) -> None:
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    with open(WORKER_LOG, "a") as log:
        subprocess.Popen(
            [UV, "run", "--quiet", "--script", os.path.abspath(__file__), "apply", guid],
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=log,
            start_new_session=True,  # detach, so Claude Code does not wait on it
        )


def hook() -> None:
    data = json.load(sys.stdin)
    guid = pane_guid()
    # Only iTerm2 panes, and not tmux inside iTerm2 (its $ITERM_SESSION_ID
    # names the pane tmux was started from, not a pane we can restore into).
    if sys.platform != "darwin" or not guid or os.environ.get("TMUX"):
        return
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    event = data["hook_event_name"]
    session_id = data["session_id"]
    need_worker = False

    with PaneLock(guid):
        record = read_json(record_path(guid))

        if event == "SessionEnd":
            reason = data.get("reason", "other")
            if record is None or record["session_id"] != session_id:
                return
            if reason in DELIBERATE_END_REASONS:
                record_path(guid).unlink()
                # The worker restores the profile's own setting for a pane
                # that no longer has a session. Skip that when the same
                # process goes on with another session.
                need_worker = reason not in SAME_PROCESS_END_REASONS
            else:
                # Killed (closed pane, iTerm2 quit, logout): keep the record so
                # the session is resumed. Never talk to iTerm2 here.
                record["ended"] = reason
                record["updated"] = time.time()
                write_json(record_path(guid), record)
        else:
            state = desired_state(event, data)
            if (
                event == "SessionStart"
                or record is None
                or record["session_id"] != session_id
            ):
                # A new session in this pane (also /clear and /resume), or one
                # started before this hook was installed.
                claude_pid = find_claude_pid()
                record = {
                    "session_id": session_id,
                    "cwd": data["cwd"],
                    "transcript_path": data.get("transcript_path", ""),
                    "iterm_session_id": os.environ["ITERM_SESSION_ID"],
                    "claude_pid": claude_pid,
                    "claude_lstart": ps(claude_pid, "lstart") if claude_pid else "",
                    "started": time.time(),
                    "state": None,
                }
                # The override may be stale (e.g. carried over by window
                # restoration), so make the worker set it again.
                applied_path(guid).unlink(missing_ok=True)
                state = state or "busy"
            if state is not None and state != record["state"]:
                record["state"] = state
                need_worker = True
            record["updated"] = time.time()
            write_json(record_path(guid), record)

    if need_worker:
        spawn_worker(guid)


# ----------------------------------------------------------------- worker


def apply(guid: str) -> None:
    """Make the pane's quit prompt match its record. Runs under uv."""
    import asyncio

    # One worker at a time per pane. A worker that waited here re-reads the
    # record, so whichever runs last applies the newest state.
    with PaneLock(guid, "worker-lock"):
        with PaneLock(guid):
            record = read_json(record_path(guid))
            want = record["state"] if record else "ended"
            have = applied_path(guid).read_text() if applied_path(guid).exists() else None
        if want == have or (want == "ended" and have is None):
            return
        if not iterm2_running():
            return
        asyncio.run(set_prompt(guid, want))
        with PaneLock(guid):
            if want == "ended":
                applied_path(guid).unlink(missing_ok=True)
            else:
                applied_path(guid).write_text(want)


async def set_prompt(guid: str, want: str) -> None:
    import iterm2

    connection = await iterm2.Connection.async_create()
    app = await iterm2.async_get_app(connection)
    session = app.get_session_by_id(guid)
    if session is None:
        # The pane is gone.
        return
    if want == "idle":
        value = PROMPT_NEVER
    else:
        # The shared profile's own setting. The session's profile is a
        # session-local copy once anything has been overridden in it.
        profile = (await session.async_get_profile()).all_properties
        shared_guid = profile.get("Original Guid") or profile["Guid"]
        shared = await iterm2.PartialProfile.async_query(
            connection, guids=[shared_guid], properties=[PROMPT_KEY]
        )
        value = shared[0].all_properties[PROMPT_KEY]
    change = iterm2.LocalWriteOnlyProfile()
    change._simple_set(PROMPT_KEY, value)
    await session.async_set_profile_properties(change)
    print(f"{time.strftime('%F %T')} {guid}: {want} -> {PROMPT_KEY} = {value}", flush=True)


# ----------------------------------------------------------------- resume


def shell_resume() -> None:
    """Print the command that resumes this pane's interrupted session, if any.

    Run from ~/.profile as: eval "$(claude-session-restore.py shell-resume)"
    """
    guid = pane_guid()
    if not guid or not record_path(guid).exists():
        return
    with PaneLock(guid):
        record = read_json(record_path(guid))
        if record is None or claude_alive(record):
            return
        record_path(guid).unlink()
        applied_path(guid).unlink(missing_ok=True)
    if not os.path.isdir(record["cwd"]):
        print(f"echo {shlex.quote('claude-session-restore: not resuming ' + record['session_id'] + ', ' + record['cwd'] + ' is gone')} >&2")
        return
    print(f"echo {shlex.quote('Resuming Claude Code session ' + record['session_id'])}")
    print(resume_command(record))


def list_orphans() -> None:
    found = orphans()
    if not found:
        print("No interrupted Claude Code sessions.")
        return
    for guid, record in found:
        age = (time.time() - record.get("updated", 0)) / 3600
        print(f"{record['session_id']}  {age:6.1f}h ago  {record['cwd']}")


def open_orphans(session_ids: list) -> None:
    """Resume interrupted sessions in new tabs of the current window."""
    import asyncio

    found = orphans()
    if session_ids != ["--all"]:
        found = [(guid, record) for guid, record in found if record["session_id"] in session_ids]
        missing = set(session_ids) - {record["session_id"] for _, record in found}
        if missing:
            sys.exit(f"Not interrupted sessions: {' '.join(sorted(missing))}")
    if not found:
        print("No interrupted Claude Code sessions.")
        return
    asyncio.run(open_tabs([record for _, record in found]))
    for guid, _ in found:
        with PaneLock(guid):
            record_path(guid).unlink(missing_ok=True)
            applied_path(guid).unlink(missing_ok=True)


async def open_tabs(records: list) -> None:
    import iterm2

    connection = await iterm2.Connection.async_create()
    app = await iterm2.async_get_app(connection)
    window = app.current_terminal_window
    for record in records:
        if window is None:
            window = await iterm2.Window.async_create(connection)
            session = window.current_tab.sessions[0]
        else:
            # current_session can still be None right after creation.
            session = (await window.async_create_tab()).sessions[0]
        await session.async_send_text(resume_command(record) + "\n")
        print(f"Resuming {record['session_id']} in {record['cwd']}")


def rerun_under_uv() -> None:
    os.execv(UV, [UV, "run", "--quiet", "--script", os.path.abspath(__file__), *sys.argv[1:]])


def main() -> None:
    usage = f"usage: {sys.argv[0]} hook | apply GUID | shell-resume | list | open --all|SESSION_ID..."
    if len(sys.argv) < 2:
        sys.exit(usage)
    command, args = sys.argv[1], sys.argv[2:]
    if command == "hook":
        hook()
    elif command == "apply" and len(args) == 1:
        apply(args[0])
    elif command == "shell-resume":
        shell_resume()
    elif command == "list":
        list_orphans()
    elif command == "open" and args:
        try:
            import iterm2  # noqa: F401
        except ModuleNotFoundError:
            rerun_under_uv()
        open_orphans(args)
    else:
        sys.exit(usage)


if __name__ == "__main__":
    main()
