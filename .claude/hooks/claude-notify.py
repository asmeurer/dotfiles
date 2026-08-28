#!/usr/bin/env python3
"""
Claude Code desktop notifications that do not pile up (macOS / iTerm2).

Replacement for Claude Code's built-in iTerm2 notifications, whose one flaw is
that they accumulate in Notification Center forever. This hook posts through
terminal-notifier with a per-session ``-group``, so:

- each Claude Code session ever has at most ONE notification showing: a newer
  event replaces the older one (terminal-notifier removes old notifications
  with the same group), and
- the notification is removed the moment you send a new message to that
  session (UserPromptSubmit) or the session ends (SessionEnd).

Clicking a notification focuses the exact iTerm2 session the hook ran in
(via AppleScript and the UUID in $ITERM_SESSION_ID), falling back to just
activating iTerm2 when that is not available.

Wire it up in ~/.claude/settings.json (hooks):

    Notification, Stop, SessionEnd, UserPromptSubmit:
        python3 ~/.claude/hooks/claude-notify.py <HookEventName>
    PreToolUse (matcher "AskUserQuestion|ExitPlanMode"):
        python3 ~/.claude/hooks/claude-notify.py PreToolUse

and set "preferredNotifChannel": "notifications_disabled" so iTerm2 stops
posting its own (un-clearable) copies. Requires ``brew install
terminal-notifier``. On first click macOS will ask once to let
terminal-notifier control iTerm2 (Automation permission).

Internal subcommand: ``claude-notify.py focus <session-uuid>`` is what the
notification's click action runs.
"""

import json
import os
import re
import shlex
import subprocess
import sys
from pathlib import Path

TERMINAL_NOTIFIER = "/opt/homebrew/bin/terminal-notifier"
ITERM2_BUNDLE_ID = "com.googlecode.iterm2"
# Notification sound (a name from /System/Library/Sounds, "default", or ""
# for silent). Used for every notification kind.
SOUND = "default"
# Only this much of the transcript tail is read to find Claude's last message.
TRANSCRIPT_TAIL_BYTES = 256 * 1024
MESSAGE_MAX_CHARS = 160
# Hard cap on how long any terminal-notifier / osascript call may take, so a
# wedged notifier can never hold up Claude Code.
SUBPROCESS_TIMEOUT = 10

FOCUS_APPLESCRIPT = """
on run argv
  set target to item 1 of argv
  tell application "iTerm2"
    -- Find the session, recording *stable* coordinates for it: the window's
    -- id and the tab's position. Selecting a window reorders the `windows`
    -- list (it is ordered front to back), and AppleScript resolves loop
    -- references like `tab 6 of window 2` lazily, so selecting through the
    -- loop variables would re-resolve them against the reshuffled list and
    -- select the matching tab of some *other* window.
    set winID to missing value
    set tabIdx to 0
    repeat with wi from 1 to count of windows
      repeat with ti from 1 to count of tabs of window wi
        repeat with s in sessions of (tab ti of window wi)
          if id of s is target then
            set winID to id of window wi
            set tabIdx to ti
            exit repeat
          end if
        end repeat
        if winID is not missing value then exit repeat
      end repeat
      if winID is not missing value then exit repeat
    end repeat
    if winID is missing value then
      activate
      return "not found"
    end if
    set targetWindow to (first window whose id is winID)
    set targetTab to tab tabIdx of targetWindow
    select targetTab
    select (first session of targetTab whose id is target)
    select targetWindow
    activate
    return "focused"
  end tell
end run
"""


def group_for(session_id: str) -> str:
    return f"claude-code-{session_id}"


def run(cmd, **kwargs):
    return subprocess.run(
        cmd, capture_output=True, text=True, timeout=SUBPROCESS_TIMEOUT, **kwargs
    )


def remove_notification(session_id: str) -> None:
    run([TERMINAL_NOTIFIER, "-remove", group_for(session_id)])


def git_branch(cwd: str) -> str:
    if not cwd or not os.path.isdir(cwd):
        return ""
    result = run(["git", "-C", cwd, "rev-parse", "--abbrev-ref", "HEAD"])
    return result.stdout.strip() if result.returncode == 0 else ""


def squash(text: str, limit: int = MESSAGE_MAX_CHARS) -> str:
    text = re.sub(r"\s+", " ", text).strip()
    if len(text) > limit:
        text = text[: limit - 1].rstrip() + "…"
    return text


def last_assistant_text(transcript_path: str) -> str:
    """Return the text of Claude's most recent message in the transcript."""
    if not transcript_path or not os.path.isfile(transcript_path):
        return ""
    path = Path(transcript_path)
    size = path.stat().st_size
    with open(path, "rb") as f:
        f.seek(max(0, size - TRANSCRIPT_TAIL_BYTES))
        lines = f.read().decode("utf-8", errors="replace").splitlines()
    for line in reversed(lines):
        line = line.strip()
        if not line:
            continue
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            # The first line read may be a partial record cut by the seek.
            continue
        if entry.get("type") != "assistant" or entry.get("isSidechain"):
            continue
        content = entry.get("message", {}).get("content", [])
        if isinstance(content, str):
            return content
        texts = [
            block.get("text", "")
            for block in content
            if isinstance(block, dict) and block.get("type") == "text"
        ]
        if texts:
            return "\n".join(texts)
    return ""


def click_command(iterm_session_id: str) -> str:
    """Shell command terminal-notifier runs when the notification is clicked."""
    uuid = iterm_session_id.rsplit(":", 1)[-1] if iterm_session_id else ""
    if not uuid:
        return ""
    return shlex.join([sys.executable, os.path.abspath(__file__), "focus", uuid])


def focus(uuid: str) -> None:
    result = run(["osascript", "-", uuid], input=FOCUS_APPLESCRIPT)
    if result.returncode != 0:
        # AppleScript unavailable (e.g. Automation permission denied): at
        # least bring iTerm2 forward.
        run(["open", "-b", ITERM2_BUNDLE_ID])
        sys.stderr.write(result.stderr)
        sys.exit(result.returncode)


def notify(session_id: str, cwd: str, kind: str, message: str) -> None:
    folder = os.path.basename(cwd.rstrip("/")) if cwd else ""
    branch = git_branch(cwd)
    title = "Claude Code" + (f" · {folder}" if folder else "")
    if branch:
        title += f" ({branch})"
    cmd = [
        TERMINAL_NOTIFIER,
        "-title", title,
        "-subtitle", kind,
        "-message", message or kind,
        "-group", group_for(session_id),
    ]
    if SOUND:
        cmd += ["-sound", SOUND]
    click = click_command(os.environ.get("ITERM_SESSION_ID", ""))
    if click:
        cmd += ["-execute", click]
    else:
        cmd += ["-activate", ITERM2_BUNDLE_ID]
    run(cmd)


def handle(event: str, data: dict) -> None:
    session_id = data.get("session_id", "")
    cwd = data.get("cwd", "") or os.getcwd()
    if not session_id:
        return

    if event in ("UserPromptSubmit", "SessionEnd"):
        remove_notification(session_id)
        return

    if event == "Stop":
        # stop_hook_active means Claude is continuing because a Stop hook
        # asked it to; the real end of the turn will fire another Stop.
        if data.get("stop_hook_active"):
            return
        notify(session_id, cwd, "Done",
               squash(last_assistant_text(data.get("transcript_path", ""))))
        return

    if event == "Notification":
        ntype = data.get("notification_type", "")
        message = data.get("message", "")
        if ntype == "auth_success":
            return
        kind = {
            "permission_prompt": "Needs permission",
            "idle_prompt": "Waiting for input",
            "elicitation_dialog": "Needs input",
        }.get(ntype, data.get("title") or "Notification")
        notify(session_id, cwd, kind, squash(message))
        return

    if event == "PreToolUse":
        tool = data.get("tool_name", "")
        tool_input = data.get("tool_input", {}) or {}
        if tool == "AskUserQuestion":
            questions = tool_input.get("questions") or []
            text = questions[0].get("question", "") if questions else ""
            notify(session_id, cwd, "Question", squash(text))
        elif tool == "ExitPlanMode":
            notify(session_id, cwd, "Plan ready", "Claude has a plan for you to review")
        return


def main() -> None:
    if len(sys.argv) < 2:
        sys.exit(f"usage: {sys.argv[0]} <HookEventName> | focus <session-uuid>")
    if sys.argv[1] == "focus":
        focus(sys.argv[2])
        return
    data = json.load(sys.stdin) if not sys.stdin.isatty() else {}
    handle(sys.argv[1], data)


if __name__ == "__main__":
    main()
