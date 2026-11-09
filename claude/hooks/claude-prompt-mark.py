#!/usr/bin/env python3
"""
UserPromptSubmit hook: drop an iTerm2 mark in the scrollback each time a
prompt is submitted to Claude Code, so Cmd-Shift-Up/Down (Edit > Marks and
Annotations > Jump to Previous/Next Mark) jumps between your prompts, the same
way it jumps between shell prompts with iTerm2 shell integration.

Claude Code has no built-in support for shell-integration marks (OSC 133 /
OSC 1337;SetMark), so we emit OSC 1337;SetMark ourselves.

Hooks run without a controlling terminal (/dev/tty is "Device not
configured"), and their stdout is captured by Claude Code, so we find the tty
of the nearest ancestor process that has one (the claude process) and write
the escape code to that device directly, in a single write().

The mark is placed at wherever the terminal cursor is when the write lands,
which is around the bottom of Claude's input area at submission time, so it
lands at (or within a few lines of) the submitted prompt.

This only works with the classic renderer ("tui": "default"). The fullscreen
renderer uses the alternate screen, which has no scrollback to mark.

Only iTerm2 understands OSC 1337, so this does nothing in other terminals.
Inside tmux the sequence is wrapped in a DCS passthrough (requires
`set -g allow-passthrough on`).
"""

import os
import subprocess
import sys

SET_MARK = "\033]1337;SetMark\a"


def in_iterm2():
    return (
        os.environ.get("TERM_PROGRAM") == "iTerm.app"
        or os.environ.get("LC_TERMINAL") == "iTerm2"
    )


def find_tty():
    """Return the /dev path of the tty of the nearest ancestor that has one."""
    pid = os.getppid()
    while pid > 1:
        out = subprocess.run(
            ["ps", "-o", "ppid=,tty=", "-p", str(pid)],
            capture_output=True, text=True, check=True,
        ).stdout.split()
        ppid, tty = int(out[0]), out[1]
        if tty not in ("??", "?", "-"):
            return tty if tty.startswith("/dev/") else "/dev/" + tty
        pid = ppid
    return None


def main():
    # Drain the hook's JSON payload; we don't need anything from it.
    sys.stdin.read()

    if not in_iterm2():
        return

    seq = SET_MARK
    if os.environ.get("TMUX"):
        seq = "\033Ptmux;" + seq.replace("\033", "\033\033") + "\033\\"

    tty = find_tty()
    if tty is None:
        return

    fd = os.open(tty, os.O_WRONLY | os.O_NOCTTY)
    try:
        os.write(fd, seq.encode())
    finally:
        os.close(fd)


if __name__ == "__main__":
    main()
