#!/usr/bin/env python3
"""Claude Code hook: keep the active claude-swap account on the
primary (asmeurer@gmail.com) and bridge to the fallback
(aaronmeurer@gmail.com) only when the primary's 5-hour budget is about to
run dry.

SOURCE_EMAIL is the *primary* account; TARGET_EMAIL is the fallback. Both
decisions are driven off a single ``claude-swap --list --json`` call, which
reports each account's 5-hour and 7-day usage windows (the used percentage
plus the fixed ``resetsAt`` timestamp for each).

The 5-hour window is a rolling budget with a *fixed* reset time: it can be
spent to 100% hours before it resets, and when it is, ``usageStatus`` still
reads "ok" (that field tracks the weekly/overall state, not the 5-hour
wall). So neither the reset clock nor usageStatus is a usable "about to be
walled" signal. Instead we use the same burn-rate projection the status line
shows -- projected minutes until the 5-hour budget hits 100% at the average
rate since the window opened:

  * Switch AWAY (primary -> fallback) when that projection drops under
    AWAY_PROJECTED_EMPTY_MINUTES minutes (about to run dry), when the 5-hour
    window's free headroom falls to AWAY_MIN_FREE_PCT regardless of what the
    projection says, OR whenever the primary's usageStatus is not "ok"
    (already walled, e.g. weekly cap). The absolute floor exists because the
    projection is an *average* over the window: a burst late in a mostly-idle
    window empties the budget far faster than the average predicts, and the
    average keeps reporting a comfortable margin right up to the wall.

  * Switch BACK (fallback -> primary) once the primary's 5-hour window
    has reset. The main, fetch-independent signal is wall-clock time: the
    5-hour ``resetsAt`` is a fixed timestamp, recorded when we switch away,
    so once ``now`` passes it the window has definitely reset -- no need to
    re-fetch the primary's usage, which as the *inactive* account is often
    "unavailable" (its usage-API token gets rate-limited). A secondary signal
    still fires when the inactive usage *is* fetchable and its 5-hour free
    headroom has jumped back to RETURN_FRESH_FREE_PCT. Either triggers the
    return; we hold only if we can positively see the weekly window is walled.

    Note this returns as soon as the primary is usable, *not* when the
    fallback runs low -- in the typical case the fallback still has plenty of
    headroom left when we switch back. That is deliberate: the fallback is a
    bridge across the primary's 5-hour gap, and the primary is the account we
    want to be on whenever it is available.

Fable has its own, separate budget: a per-model *weekly* bucket that the usage
API reports as a named ``scoped`` window (``usage.scoped[] .name == "Fable"``)
and that claude-swap passes through in --list --json. It is spent independently
of the 5-hour and 7-day windows, so the primary can sit at 5% Fable free while
its 5-hour window is barely touched. Three things follow, and each is why the
Fable rule is *not* a copy of the 5-hour rule:

  * Trigger on a plain free-% floor (FABLE_AWAY_MIN_FREE_PCT), never on a
    burn-rate projection. The bucket resets with the 7-day window, so the
    projection would say "won't last the week" days early -- and acting on it
    would park us on the fallback for those days.

  * Only when Fable is actually the model in use. The hook payload does not
    carry the model, so each invocation reads the tail of its session's
    transcript (``transcript_path``) and stamps ``fable_last_seen_ts`` in the
    state file from the latest main-session assistant entry, or from genuinely
    recent sidechain Fable entries. Any session's stamp counts, for
    FABLE_ACTIVE_WINDOW_SECONDS -- the accounts are global, and the throttle
    means the session that happens to win the 60-second race is often not the
    one running Fable.

    A *refused* request counts as use too, and this is not an edge case: when
    the bucket is already empty the request never reaches a model, so the
    transcript gets a ``<synthetic>`` "out of usage credits ... Fable" entry and
    no ``claude-fable-5`` entry at all. A session that starts on Fable while
    walled would otherwise be invisible to the rule that exists for it. That
    refusal also stamps ``fable_walled_ts`` with the active account, which
    forces the away rule only when the primary produced the refusal. It is
    ground truth from the API, while the percentage comes from a snapshot up to
    ~3 minutes stale.

  * Only when the fallback can actually serve Fable (usageStatus not limiting,
    and its own Fable bucket above FABLE_TARGET_MIN_FREE_PCT). A Fable-spent
    primary is still perfectly good for Opus/Sonnet, so unlike the 5-hour wall
    -- which makes the account useless for everything -- leaving pays only if
    the destination has Fable to spend. When that check refuses (the fallback
    needs a re-login, say) the hook raises a desktop notification at most every
    BLOCKED_NOTIFY_INTERVAL_SECONDS: from the outside this case is
    indistinguishable from the hook being broken, so it must not be silent.

  The return side needs the mirror of that rule, or the two oscillate: the
  5-hour signal says "primary is fine, go back", the Fable rule says "primary
  is spent, leave" and we bounce every check. So while Fable is in use we HOLD
  on the fallback whenever the primary's Fable bucket is still spent -- read
  live when the primary's usage is fetchable, otherwise from the
  ``pending_fable_reset_at`` recorded on the way out. A Fable-only departure can
  return after MIN_SWITCH_INTERVAL_SECONDS once Fable is no longer active;
  otherwise the unrelated 5-hour reset would strand the fallback for hours.
  MIN_SWITCH_INTERVAL_SECONDS is a backstop under all of this: no two switches
  closer together than that (except an outright usageStatus wall, which must not
  wait).

  Not handled, because there is nowhere to go: the *fallback* running out of
  Fable while we are on it. With two accounts that is simply the end of Fable
  for the week; the hook keeps the return-to-primary behavior it always had.

Caveats: the usage snapshot behind every decision is served from claude-swap's
store with a 180-second freshness floor (poll_policy.SERVE_TTL_S), dropping to
~60s for the active account while it is visibly burning toward a limit -- so
even a fresh read can be a few minutes behind reality. AWAY_PROJECTED_EMPTY_MINUTES
and AWAY_MIN_FREE_PCT are both margin against that staleness plus a burst.

Wired in ~/.claude/settings.json on:

  * PostToolUse   -- fires after every tool call, so a long turn that never
                     reaches a Stop still gets evaluated. This is the trigger
                     that matters: turn-boundary-only hooks can go hours
                     without firing while the budget drains inside one turn.
  * Stop, StopFailure, SubagentStop, UserPromptSubmit, PreCompact -- turn and
    session boundaries, including the failure path (a turn that walls the
    account mid-response never cleanly Stops, but does StopFailure).

Because PostToolUse fires per tool call, CHECK_INTERVAL_SECONDS throttles the
actual work: invocations inside that window exit silently without running
claude-swap or writing a log line. The throttle clock is the mtime of
THROTTLE_PATH; a nonblocking process lock serializes that throttle decision
with switch-state mutation so concurrent sessions cannot duplicate a switch or
race while reading/writing STATE_PATH.

A mid-turn switch does take effect on the running session -- claude-swap
rewrites ~/.claude/.credentials.json on every switch specifically to bump its
mtime and force Claude Code to drop its memoized OAuth token (see
claude_swap/credentials.py::_write_oauth_credentials). No restart needed;
worst case the running session takes ~30s (the macOS Keychain cache TTL) to
pick up the new account.

Always exits 0 on expected conditions so it never blocks a turn; unexpected
errors are allowed to surface as a traceback.

TO TURN THIS OFF: set ENABLED = False below (one line, tracked in dotfiles).
That is the switch to flip once the fallback account goes away. Nothing else
needs undoing -- the hook then no-ops on every turn, and the status line's
"↩ asmeurer N% free" note self-disables because it only renders while the
fallback account is the active one. For a temporary, per-shell disable
instead, export CLAUDE_AUTO_SWAP_DISABLED=1.
"""
import fcntl
import json
import os
import shutil
import subprocess
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

# Master on/off switch. Flip to False to retire auto-swapping entirely (e.g.
# when the fallback account is gone) without unwiring the hook from
# settings.json -- keeping the switch in the script means everything the hook
# needs in order to come back is still here, wired up and ready.
#
# Currently off: the aaronmeurer@gmail.com fallback account is no longer
# active, so there is nothing to bridge to. Flip back to True if it is ever
# reenabled; nothing else needs redoing.
ENABLED = False

SOURCE_EMAIL = "asmeurer@gmail.com"      # primary account
TARGET_EMAIL = "aaronmeurer@gmail.com"   # fallback account
# Short names for log/notification lines.
SOURCE_NAME = SOURCE_EMAIL.split("@")[0]
TARGET_NAME = TARGET_EMAIL.split("@")[0]
# Leave the primary when its 5-hour budget is projected to empty within this
# many minutes at the current average burn rate (the status line's estimate).
AWAY_PROJECTED_EMPTY_MINUTES = 15.0
# Hard floor: leave the primary once its 5-hour window has this little free
# left, whatever the projection says. The projection is a window *average*, so a
# burst late in an otherwise-idle window keeps reporting a roomy margin while the
# budget actually falls off a cliff -- the 2026-08-07 near-miss went 7% free
# ("empties in ~16m", stay) to 1% free in four minutes. This floor is what
# guarantees the switch lands before the wall, at the cost of leaving a few
# percent of the window unspent.
AWAY_MIN_FREE_PCT = 8.0
# The 5-hour window length, used to derive elapsed time for the burn-rate
# projection (mirrors the status line's 300-minute constant).
FIVE_HOUR_WINDOW_MINS = 300.0
# Name of Fable's per-model weekly bucket in the usage API's ``scoped`` list
# (matched case-insensitively). Same name the status line looks for.
FABLE_SCOPED_NAME = "Fable"
# Leave the primary while running Fable once its Fable weekly bucket has this
# little left. No projection here, deliberately -- see the module docstring: a
# weekly bucket's burn rate would evict us days early. This is a small floor
# because the cost of leaving is high (the bucket only refills at the 7-day
# reset), so we spend the primary's Fable budget nearly to the end first.
FABLE_AWAY_MIN_FREE_PCT = 5.0
# ...and only if the fallback has at least this much Fable budget left, i.e.
# meaningfully more than the floor we just left over. Switching to an equally
# spent account trades a working Opus/Sonnet account for nothing.
FABLE_TARGET_MIN_FREE_PCT = 15.0
# How long a Fable sighting in any session keeps the Fable rules armed. Covers
# the gap between transcript stamps (a long thinking turn writes nothing) and
# lets a Fable session that is currently losing the throttle race still be seen.
FABLE_ACTIVE_WINDOW_SECONDS = 900.0
# Don't rewrite the state file more often than this just to refresh the Fable
# stamp -- the stamp is checked against a 15-minute window, so 30s is plenty of
# resolution, and this path runs on every single PostToolUse.
FABLE_STAMP_MIN_INTERVAL_SECONDS = 30.0
# How much of a session transcript to read when looking for the current model.
# Only the tail matters, and one large tool result can push the last assistant
# entry well back, so this is generous.
TRANSCRIPT_TAIL_BYTES = 256 * 1024
# How many assistant entries back to look for Fable. More than one because the
# last entry may be a subagent (isSidechain) on a different model than the main
# session. The latest main-session entry decides the current model; sidechain
# entries count only while they are fresh by their own transcript timestamp.
TRANSCRIPT_SCAN_ASSISTANT_ENTRIES = 10
# The model field Claude Code stamps on harness-generated assistant entries
# (errors, refusals) that never reached a model.
SYNTHETIC_MODEL = "<synthetic>"
# Phrases in a ``<synthetic>`` entry that, together with the model name, mean
# the request was refused for that model's credits -- e.g. "You're out of usage
# credits. Run /usage-credits to keep using Fable 5 or /model to switch models."
FABLE_WALL_MARKERS = ("out of usage credits", "usage limit")
# How long such a refusal counts as "walled right now". Transcripts are
# append-only and re-read from the tail on every invocation, so this age check
# is what stops one old refusal from arming the rule permanently.
FABLE_WALL_RECENT_SECONDS = 600.0
# How often, at most, to raise a desktop notification about wanting to leave the
# primary for Fable but having nowhere to go (e.g. the fallback needs a
# re-login). Without a floor this fires every evaluation, i.e. once a minute.
BLOCKED_NOTIFY_INTERVAL_SECONDS = 1800.0
# Floor on how often accounts may be swapped. A backstop against a decision
# rule oscillating (each swap rewrites the macOS Keychain, which can block or
# prompt), not a normal part of any rule. Bypassed for a usageStatus wall,
# where waiting means running turns against a walled account.
MIN_SWITCH_INTERVAL_SECONDS = 300.0
# Minimum seconds between real evaluations. PostToolUse fires per tool call, but
# the underlying usage snapshot only refreshes every ~60-180s
# (poll_policy.SERVE_TTL_S), so checking more often than this buys no new data
# and just adds latency to every tool result.
CHECK_INTERVAL_SECONDS = 60.0
# Timeouts on the claude-swap subprocesses. Without these a hung usage API would
# stall every tool result at PostToolUse cadence. The switch gets a longer budget
# than the read: it writes the macOS Keychain, which can prompt or block.
LIST_TIMEOUT_SECONDS = 10.0
SWITCH_TIMEOUT_SECONDS = 30.0
NOTIFY_TIMEOUT_SECONDS = 5.0
# Return to the primary once its 5-hour window reads at least this % free again
# (i.e. it has reset -- a fresh window reads ~100% free, a spent one ~0%). This
# is the secondary return signal, used only when the inactive account's usage is
# fetchable; the main signal is wall-clock past the recorded reset time.
RETURN_FRESH_FREE_PCT = 90.0
# Hold on the fallback if we can positively see the primary's weekly window has
# this little free left -- it would be walled until the 7-day reset, days away.
WEEKLY_WALL_FREE_PCT = 2.0
# usageStatus values that mean the usage API did not return a usable status for
# an inactive account, not that the account is positively limit-walled.
USAGE_UNAVAILABLE_STATUSES = {"unavailable"}
# Known install location, used first so the hook works even if the spawning
# shell's PATH lacks ~/.local/bin; falls back to PATH lookup if it moves.
CLAUDE_SWAP_PATH = str(Path.home() / ".local" / "bin" / "claude-swap")
# Machine-local runtime state: the most recent switch (audit), the reset
# timestamps captured when we switched away -- ``pending_source_reset_at`` (the
# primary's 5-hour resetsAt) and ``pending_fable_reset_at`` (its Fable weekly
# resetsAt, only when that bucket was spent) -- used to time the return without
# re-fetching the primary's usage, plus the away reason and Fable stamps. Fable
# wall stamps include the active account, so a fallback refusal cannot later be
# mistaken for a primary refusal.
# Deliberately not in Dropbox -- it is per-machine, ephemeral state.
STATE_PATH = Path.home() / ".claude" / "auto-swap-state.json"
# Throttle clock for CHECK_INTERVAL_SECONDS. Deliberately a *separate*, empty
# file whose mtime is the timestamp, rather than a key in STATE_PATH. Access to
# it is protected by LOCK_PATH, which also protects STATE_PATH mutation.
THROTTLE_PATH = Path.home() / ".claude" / "auto-swap-last-check"
# Nonblocking inter-process lock. If another hook is already evaluating, this
# invocation silently exits; the active evaluator owns the throttle stamp and
# any state-file mutation.
LOCK_PATH = Path.home() / ".claude" / "auto-swap.lock"
# Append-only audit log: one line per *non-throttled* invocation (so at most one
# per CHECK_INTERVAL_SECONDS across all sessions) recording what usage the hook
# saw and what it decided. This is the diagnostic for "it didn't switch in time":
# a steady ~60s cadence of lines through a long turn means the hook is watching
# and the thresholds are what need tuning, while a multi-minute gap during active
# work means it is not being invoked at all. Machine-local, not Dropbox.
LOG_PATH = Path.home() / ".claude" / "auto-swap.log"
LOG_MAX_BYTES = 1_000_000  # rotate (keep tail) past this size


def resolve_swap() -> str | None:
    """Locate the claude-swap executable, preferring the known absolute path."""
    if Path(CLAUDE_SWAP_PATH).is_file():
        return CLAUDE_SWAP_PATH
    return shutil.which("claude-swap")


def is_unavailable_status(status: str | None) -> bool:
    """True when usageStatus means no current data, not a known account wall."""
    return status in USAGE_UNAVAILABLE_STATUSES


def is_limiting_status(status: str | None) -> bool:
    """True when usageStatus positively indicates the account should not run."""
    return (
        status is not None
        and status != "ok"
        and not is_unavailable_status(status)
    )


def scoped_window(usage: dict | None, name: str) -> dict | None:
    """The named per-model weekly window from ``usage.scoped``, or None.

    Matched case-insensitively on the model display name (e.g. "Fable"). Older
    usage-API responses carry no ``scoped`` key at all, which reads as None.
    """
    if not isinstance(usage, dict):
        return None
    for entry in usage.get("scoped") or []:
        if isinstance(entry, dict) and str(entry.get("name", "")).lower() == name.lower():
            return entry
    return None


def free_pct(window: dict | None) -> float | None:
    """Free headroom (100 - utilization) for one usage window, or None.

    Returns None when the window or its ``pct`` is missing, so callers can
    distinguish "no data" from "0% free". Works for any window shape -- the
    top-level fiveHour/sevenDay windows and the scoped per-model ones alike.
    """
    if isinstance(window, dict) and isinstance(window.get("pct"), (int, float)):
        return 100.0 - window["pct"]
    return None


def reset_minutes(window: dict | None) -> float | None:
    """Minutes until the window's ``resetsAt``, or None if unavailable.

    ``resetsAt`` is a fixed UTC timestamp, so this is accurate even when the
    cached usage snapshot is a few minutes stale. Can be negative briefly at
    the reset boundary (resetsAt just passed but the snapshot not yet
    refreshed); callers treat that as "already reset".
    """
    if not isinstance(window, dict):
        return None
    resets_at = window.get("resetsAt")
    if not isinstance(resets_at, str):
        return None
    reset_dt = datetime.fromisoformat(resets_at)
    return (reset_dt - datetime.now(timezone.utc)).total_seconds() / 60.0


def window_free_pct(usage: dict, key: str) -> float | None:
    """Free headroom for a top-level usage window ("fiveHour"/"sevenDay")."""
    return free_pct(usage.get(key) if isinstance(usage, dict) else None)


def minutes_until_reset(usage: dict, key: str) -> float | None:
    """Minutes until a top-level usage window's reset."""
    return reset_minutes(usage.get(key) if isinstance(usage, dict) else None)


def fable_free_pct(usage: dict | None) -> float | None:
    """Free headroom in the Fable per-model weekly bucket, or None."""
    return free_pct(scoped_window(usage, FABLE_SCOPED_NAME))


def account_fable_free_pct(account: dict | None) -> float | None:
    """Fable headroom for an account, falling back to its last good snapshot.

    An *inactive* account frequently has ``usage: null`` (its usage-API token is
    rate-limited), but claude-swap keeps the last successful read in
    ``lastGoodUsage``. A weekly bucket moves slowly, so a stale reading is still
    informative -- good enough to answer "does the fallback have Fable left",
    which is all this is used for.
    """
    if not isinstance(account, dict):
        return None
    for key in ("usage", "lastGoodUsage"):
        pct = fable_free_pct(account.get(key))
        if pct is not None:
            return pct
    return None


def project_empty_minutes(usage: dict, key: str, window_mins: float) -> float | None:
    """Projected minutes until the window's budget hits 100%, or None.

    Mirrors ``project_empty_mins()`` in statusline-command.sh: extrapolates the
    average burn rate since the window opened (used_pct / elapsed) forward to
    when the remaining budget is spent. Returns None when the projection is not
    meaningful -- no usage yet, no elapsed time, missing reset clock, or the
    budget is on track to outlast the window (won't run out before it resets).
    """
    window = usage.get(key)
    if not isinstance(window, dict):
        return None
    used_pct = window.get("pct")
    if not isinstance(used_pct, (int, float)) or used_pct <= 0:
        return None
    mins_left = minutes_until_reset(usage, key)
    if mins_left is None:
        return None
    mins_elapsed = window_mins - mins_left
    if mins_elapsed <= 0:
        return None
    projected = (100.0 - used_pct) * mins_elapsed / used_pct
    projected = max(projected, 0.0)
    # Only meaningful if the budget runs out before the window resets.
    return projected if projected < mins_left else None


def acquire_evaluation_lock():
    """Acquire the process lock, or return None if another hook has it."""
    LOCK_PATH.parent.mkdir(parents=True, exist_ok=True)
    lock_file = LOCK_PATH.open("w")
    try:
        fcntl.flock(lock_file, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        lock_file.close()
        return None
    return lock_file


def throttled(now: float) -> bool:
    """True if another invocation already evaluated within CHECK_INTERVAL_SECONDS.

    Uses THROTTLE_PATH's mtime as the clock and stamps it when the caller is
    cleared to proceed. Call only while holding LOCK_PATH, which makes the
    stat/touch decision atomic across hook processes. A missing file -- first
    run after a reboot that clears the state dir -- reads as "not throttled".
    """
    try:
        if now - THROTTLE_PATH.stat().st_mtime < CHECK_INTERVAL_SECONDS:
            return True
    except FileNotFoundError:
        pass
    THROTTLE_PATH.touch()
    return False


def read_state() -> dict:
    """Load the runtime state file, or {} on the expected first-run absence."""
    try:
        return json.loads(STATE_PATH.read_text())
    except FileNotFoundError:
        return {}


def write_state(state: dict) -> None:
    STATE_PATH.write_text(json.dumps(state))


def record_switch(
    to_email: str, now: float, away: bool,
    source_reset_at: str | None = None, fable_reset_at: str | None = None,
    away_reason: str | None = None,
) -> None:
    """Persist the most recent switch (audit) and arm/clear the pending resets.

    On the AWAY switch (``away=True``) the primary's reset timestamps are stored
    so the return can be timed off wall clock alone: its 5-hour ``resetsAt``,
    and its Fable weekly ``resetsAt`` when that bucket was spent. Either may be
    None if that window had no data. The return switch clears both.
    """
    state = read_state()
    state["last_switch_ts"] = now
    state["last_switch_to"] = to_email
    for key, value in (
        ("pending_source_reset_at", source_reset_at if away else None),
        ("pending_fable_reset_at", fable_reset_at if away else None),
        ("pending_away_reason", away_reason if away else None),
    ):
        if value is not None:
            state[key] = value
        else:
            state.pop(key, None)
    write_state(state)


def arm_pending_resets(
    source_reset_at: str | None, fable_reset_at: str | None,
    away_reason: str | None = None,
) -> None:
    """Persist away-time resets before attempting a possibly slow switch."""
    state = read_state()
    if source_reset_at is not None:
        state["pending_source_reset_at"] = source_reset_at
    if fable_reset_at is not None:
        state["pending_fable_reset_at"] = fable_reset_at
    if away_reason is not None:
        state["pending_away_reason"] = away_reason
    write_state(state)


def _pending_reset(key: str) -> datetime | None:
    s = read_state().get(key)
    if not isinstance(s, str):
        return None
    return datetime.fromisoformat(s)


def get_pending_reset() -> datetime | None:
    """The primary's recorded away-time 5-hour resetsAt as a datetime, or None."""
    return _pending_reset("pending_source_reset_at")


def get_pending_fable_reset() -> datetime | None:
    """The primary's recorded away-time Fable weekly resetsAt, or None."""
    return _pending_reset("pending_fable_reset_at")


def get_pending_away_reason() -> str | None:
    """The reason recorded for the current fallback stay, if known."""
    reason = read_state().get("pending_away_reason")
    return reason if isinstance(reason, str) else None


def clear_pending_resets() -> None:
    """Drop armed pending resets (we are back on / already using the primary)."""
    state = read_state()
    if state.keys() & {
        "pending_source_reset_at",
        "pending_fable_reset_at",
        "pending_away_reason",
    }:
        state.pop("pending_source_reset_at", None)
        state.pop("pending_fable_reset_at", None)
        state.pop("pending_away_reason", None)
        write_state(state)


def entry_text(message: dict) -> str:
    """Flatten an assistant message's content blocks to plain text."""
    content = message.get("content")
    if isinstance(content, str):
        return content
    parts = [
        block["text"]
        for block in (content or [])
        if isinstance(block, dict) and isinstance(block.get("text"), str)
    ]
    return " ".join(parts)


def entry_age_seconds(entry: dict) -> float | None:
    """Seconds since a transcript entry's ``timestamp``, or None if unusable."""
    written_ts = entry_timestamp(entry)
    if written_ts is None:
        return None
    return time.time() - written_ts


def entry_timestamp(entry: dict) -> float | None:
    """Unix timestamp for a transcript entry's ``timestamp``, or None."""
    stamp = entry.get("timestamp")
    if not isinstance(stamp, str):
        return None
    try:
        written = datetime.fromisoformat(stamp.replace("Z", "+00:00"))
    except ValueError:
        return None
    if written.tzinfo is None:
        written = written.replace(tzinfo=timezone.utc)
    return written.timestamp()


def is_fable_wall_entry(entry: dict, message: dict) -> bool:
    """True for the harness's "out of usage credits" notice about Fable.

    When the Fable bucket is spent, the request never reaches a model: Claude
    Code writes an assistant entry with ``model: "<synthetic>"`` reading "You're
    out of usage credits. Run /usage-credits to keep using Fable 5 ...". The
    ``<synthetic>`` model is what makes this safe to match on -- a *real*
    assistant entry discussing the wall (this hook's own development sessions do
    exactly that) would otherwise trip it.
    """
    if message.get("model") != SYNTHETIC_MODEL:
        return False
    text = entry_text(message).lower()
    return "fable" in text and any(m in text for m in FABLE_WALL_MARKERS)


def transcript_fable_signals(transcript_path: str) -> tuple[float | None, float | None]:
    """(Fable use timestamp, Fable wall timestamp) from a transcript tail.

    Reads only the tail of the JSONL and walks it backwards over the last
    TRANSCRIPT_SCAN_ASSISTANT_ENTRIES assistant entries. The newest main-session
    entry decides the current session model. Sidechain entries can still arm the
    rule, but only if their own timestamp is recent, so an old Fable subagent
    cannot survive a later /model change.

    Two signals, because a successful Fable turn is not the only evidence:

      * in use -- the latest main-session assistant entry is Fable, a recent
        sidechain assistant entry is Fable, or there is a recent wall notice
        naming Fable. The wall notice matters on its own: a session that starts
        on Fable and is refused on its very first request never writes a
        ``claude-fable-5`` entry at all.

      * walled -- that same notice, and *recent* by the entry's own timestamp.
        Transcripts are append-only and are re-read from the tail every time, so
        without the age check one old refusal would arm the rule forever.

    Both ends of the slice can hold a partial line -- the front because the tail
    starts mid-file, the back because Claude Code may be appending as we read --
    so unparseable lines are skipped rather than treated as an error.
    """
    path = Path(transcript_path)
    try:
        size = path.stat().st_size
        with path.open("rb") as fh:
            fh.seek(max(0, size - TRANSCRIPT_TAIL_BYTES))
            tail = fh.read()
    except (FileNotFoundError, IsADirectoryError, PermissionError):
        return None, None
    lines = tail.decode("utf-8", "replace").splitlines()
    now = time.time()
    fable_seen_ts = None
    fable_wall_ts = None
    latest_main_seen = False
    seen = 0
    for line in reversed(lines):
        if seen >= TRANSCRIPT_SCAN_ASSISTANT_ENTRIES:
            break
        if '"assistant"' not in line:
            continue
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            continue  # truncated line at either end of the tail slice
        if entry.get("type") != "assistant":
            continue
        seen += 1
        message = entry.get("message")
        if not isinstance(message, dict):
            continue
        model = message.get("model")
        ts = entry_timestamp(entry)
        age = None if ts is None else now - ts
        is_sidechain = bool(entry.get("isSidechain"))
        if not is_sidechain and not latest_main_seen:
            latest_main_seen = True
            if isinstance(model, str) and "fable" in model.lower():
                fable_seen_ts = now
        elif (
            is_sidechain
            and isinstance(model, str)
            and "fable" in model.lower()
            and age is not None
            and age <= FABLE_ACTIVE_WINDOW_SECONDS
        ):
            fable_seen_ts = max(fable_seen_ts or ts, ts)
        if is_fable_wall_entry(entry, message) and age is not None:
            if age <= FABLE_WALL_RECENT_SECONDS:
                fable_wall_ts = max(fable_wall_ts or ts, ts)
                fable_seen_ts = max(fable_seen_ts or ts, ts)
    return fable_seen_ts, fable_wall_ts


def stamp_fable_use(payload: dict, wall_account: str | None = None) -> None:
    """Record Fable use (and any live Fable wall) for the Fable rules to see.

    Runs on *every* invocation, before the throttle, because the session that
    wins the throttle race is often not the one running Fable -- the stamp is
    how a Fable session that never gets to evaluate still arms the rules. Only
    the state *write* is throttled (to FABLE_STAMP_MIN_INTERVAL_SECONDS); the
    transcript read itself is a tail slice and cheap enough for the PostToolUse
    hot path.
    """
    transcript = payload.get("transcript_path")
    if not isinstance(transcript, str) or not transcript:
        return
    fable_seen_ts, fable_wall_ts = transcript_fable_signals(transcript)
    state = read_state()
    updates = {}
    if fable_seen_ts is not None:
        stored_seen_ts = state.get("fable_last_seen_ts")
        if not isinstance(stored_seen_ts, (int, float)) or fable_seen_ts > stored_seen_ts:
            updates["fable_last_seen_ts"] = fable_seen_ts
    if fable_wall_ts is not None and wall_account is not None:
        stored_wall_ts = state.get("fable_walled_ts")
        if not isinstance(stored_wall_ts, (int, float)) or fable_wall_ts > stored_wall_ts:
            updates["fable_walled_ts"] = fable_wall_ts
            updates["fable_walled_account"] = wall_account
    if not updates:
        return
    numeric_keys = [
        key for key in updates
        if isinstance(updates[key], (int, float))
    ]
    if (
        numeric_keys
        and all(
            isinstance(state.get(key), (int, float))
            and abs(updates[key] - state[key]) < FABLE_STAMP_MIN_INTERVAL_SECONDS
            for key in numeric_keys
        )
        and all(state.get(key) == updates[key] for key in updates if key not in numeric_keys)
    ):
        return  # already stamped this transcript moment; skip the write
    state.update(updates)
    write_state(state)


def _stamp_fresh(key: str, now: float, window: float) -> bool:
    last = read_state().get(key)
    return isinstance(last, (int, float)) and now - last < window


def fable_recently_active(now: float) -> bool:
    """True if any session was seen running Fable within the recency window."""
    return _stamp_fresh("fable_last_seen_ts", now, FABLE_ACTIVE_WINDOW_SECONDS)


def fable_recently_walled(now: float, account_email: str | None = None) -> bool:
    """True if a session was refused for Fable credits within the wall window.

    Ground truth, and it outranks the usage snapshot: claude-swap serves usage
    from a store with a 180s freshness floor, so the reported Fable percentage
    can still read healthy for minutes after the API has started refusing.
    """
    state = read_state()
    last = state.get("fable_walled_ts")
    if not isinstance(last, (int, float)) or now - last >= FABLE_WALL_RECENT_SECONDS:
        return False
    if account_email is None:
        return True
    return state.get("fable_walled_account") == account_email


def log(line: str) -> None:
    """Append a timestamped line to the audit log, with light tail rotation."""
    try:
        if LOG_PATH.exists() and LOG_PATH.stat().st_size > LOG_MAX_BYTES:
            tail = LOG_PATH.read_text().splitlines()[-2000:]
            LOG_PATH.write_text("\n".join(tail) + "\n")
    except FileNotFoundError:
        pass
    stamp = time.strftime("%Y-%m-%d %H:%M:%S")
    with LOG_PATH.open("a") as fh:
        fh.write(f"{stamp} {line}\n")


def notify(msg: str, to_stderr: bool = True) -> None:
    """Best-effort macOS notification plus a stderr line for the transcript."""
    if to_stderr:
        print(msg, file=sys.stderr)
    if shutil.which("osascript"):
        try:
            subprocess.run(
                ["osascript", "-e",
                 f'display notification "{msg}" with title "claude-swap"'],
                capture_output=True, timeout=NOTIFY_TIMEOUT_SECONDS,
            )
        except subprocess.TimeoutExpired:
            log(f"notification timed out after {NOTIFY_TIMEOUT_SECONDS:.0f}s")


def switch_too_soon(now: float) -> float | None:
    """Seconds still to wait under MIN_SWITCH_INTERVAL_SECONDS, or None if clear.

    A backstop only: every rule here is supposed to be stable on its own, so a
    suppression firing means two rules are disagreeing and the log should say
    so rather than have the accounts flap at the throttle's cadence.
    """
    last = read_state().get("last_switch_ts")
    if not isinstance(last, (int, float)):
        return None
    waited = now - last
    return None if waited >= MIN_SWITCH_INTERVAL_SECONDS else (
        MIN_SWITCH_INTERVAL_SECONDS - waited
    )


def do_switch(
    swap: str, to_email: str, now: float, success_msg: str,
    source_reset_at: str | None = None, fable_reset_at: str | None = None,
    away_reason: str | None = None,
    urgent: bool = False,
) -> str:
    """Run claude-swap --switch-to; record + notify on success, notify on fail.

    ``source_reset_at``/``fable_reset_at`` (set only on the AWAY switch) arm the
    pending resets before the subprocess starts. If claude-swap times out after
    changing credentials, later fallback evaluations still know when to return.

    ``urgent`` bypasses the minimum-interval backstop, for the case where
    staying put means burning turns against an account that is already walled.

    Returns "" on success, or a short parenthetical for the audit log saying
    why no switch happened.
    """
    if not urgent:
        wait = switch_too_soon(now)
        if wait is not None:
            log(f"switch to {to_email} suppressed: {wait:.0f}s left of min interval")
            return f" [held {wait:.0f}s by min switch interval]"
    away = to_email == TARGET_EMAIL
    if away:
        arm_pending_resets(source_reset_at, fable_reset_at, away_reason)
    try:
        sw = subprocess.run(
            [swap, "--switch-to", to_email], capture_output=True, text=True,
            timeout=SWITCH_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired:
        log(
            f"switch to {to_email} timed out after "
            f"{SWITCH_TIMEOUT_SECONDS:.0f}s"
        )
        return " [switch timed out]"
    if sw.returncode == 0:
        record_switch(
            to_email, now, away,
            source_reset_at=source_reset_at, fable_reset_at=fable_reset_at,
            away_reason=away_reason,
        )
        notify(success_msg)
        return ""
    if away:
        clear_pending_resets()
    notify(
        f"Tried to switch to {to_email} but claude-swap failed: "
        f"{sw.stderr.strip() or sw.stdout.strip()}"
    )
    return " [claude-swap failed]"


def fable_switch_blocker(target: dict | None) -> str | None:
    """Why the fallback cannot take a Fable-driven switch, or None if it can.

    Unlike a 5-hour wall -- which leaves the primary useless for every model --
    a spent Fable bucket leaves it perfectly good for Opus/Sonnet. So this
    switch is only worth making against *positive* evidence that the fallback
    can serve Fable; absence of data is a blocker, not a green light.
    """
    if target is None:
        return "no fallback account"
    status = target.get("usageStatus")
    if is_limiting_status(status):
        return f"fallback usageStatus={status}"
    free = account_fable_free_pct(target)
    if free is None:
        return "fallback Fable usage unknown"
    if free < FABLE_TARGET_MIN_FREE_PCT:
        return f"fallback Fable only {free:.0f}% free"
    return None


def notify_fable_blocked(blocker: str, detail: str, now: float) -> None:
    """Tell the user, at most every BLOCKED_NOTIFY_INTERVAL_SECONDS, that Fable
    is spent on the primary and the fallback cannot take over.

    This case is otherwise invisible -- it looks exactly like the hook not
    working, which is how it was first reported. Notification only, no stderr:
    it can fire on UserPromptSubmit, where hook output has a way of landing in
    the session, and a nag every half hour does not belong in the transcript.
    """
    if _stamp_fresh("last_blocked_notify_ts", now, BLOCKED_NOTIFY_INTERVAL_SECONDS):
        return
    state = read_state()
    state["last_blocked_notify_ts"] = now
    write_state(state)
    notify(
        f"Fable is out on {SOURCE_EMAIL} ({detail}) but auto-swap cannot move: "
        f"{blocker}.",
        to_stderr=False,
    )


def handle_source_active(
    swap: str, usage: dict, status: str | None, now: float,
    fable_active: bool, fable_walled: bool, target: dict | None,
) -> str:
    """On the primary: switch away when the 5h budget is about to empty, when
    Fable is in use and its weekly bucket is spent, or if walled.

    Returns a short decision string for the audit log.
    """
    # Capture the primary's reset timestamps now (it is the active, freshly-read
    # account) so the return can be timed off wall clock even if its usage later
    # reads "unavailable" as the inactive account. The Fable one is armed only
    # when that bucket is actually spent -- it is what holds the return back
    # while Fable is in use, and an unspent bucket must not hold anything.
    fh = usage.get("fiveHour")
    reset_at = fh.get("resetsAt") if isinstance(fh, dict) else None
    fable_window = scoped_window(usage, FABLE_SCOPED_NAME)
    fable_free = free_pct(fable_window)
    # A live refusal outranks the percentage: the snapshot behind ``fable_free``
    # can be up to ~3 minutes old, so it still reads healthy for a while after
    # the API has actually started saying no.
    fable_spent = fable_walled or (
        fable_free is not None and fable_free <= FABLE_AWAY_MIN_FREE_PCT
    )
    fable_detail = (
        "walled: out of credits" if fable_walled
        else f"at {fable_free:.0f}% free" if fable_free is not None
        else "spent"
    )
    fable_reset_at = None
    if fable_spent and isinstance(fable_window, dict) and isinstance(
        fable_window.get("resetsAt"), str
    ):
        fable_reset_at = fable_window["resetsAt"]

    def leave(
        reason_msg: str, label: str, away_reason: str, urgent: bool = False
    ) -> str:
        note = do_switch(
            swap, TARGET_EMAIL, now, reason_msg,
            source_reset_at=reset_at, fable_reset_at=fable_reset_at,
            away_reason=away_reason,
            urgent=urgent,
        )
        return f"AWAY->{TARGET_NAME} ({label}){note}"

    if status is not None and status != "ok":
        return leave(
            f"{SOURCE_EMAIL} usageStatus={status} (walled) "
            f"-- switched to {TARGET_EMAIL}.",
            f"usageStatus={status}", "status", urgent=True,
        )
    # Floor first: it is the signal that survives a burst the window-average
    # projection cannot see, and it fires even when the projection is None
    # (a mostly-idle window whose average says the budget outlasts the reset).
    free = window_free_pct(usage, "fiveHour")
    if free is not None and free <= AWAY_MIN_FREE_PCT:
        return leave(
            f"{SOURCE_EMAIL} 5-hour budget down to {free:.0f}% free "
            f"-- switched to {TARGET_EMAIL}.",
            f"5h at {free:.0f}% free", "five_hour",
        )

    proj = project_empty_minutes(usage, "fiveHour", FIVE_HOUR_WINDOW_MINS)
    if proj is not None and proj <= AWAY_PROJECTED_EMPTY_MINUTES:
        return leave(
            f"{SOURCE_EMAIL} 5-hour budget ~{proj:.1f}m from empty at current "
            f"burn -- switched to {TARGET_EMAIL}.",
            f"5h empties in ~{proj:.1f}m", "five_hour",
        )

    # The 5-hour window is fine. Fable draws on its own weekly bucket, so it can
    # still be spent -- but only leave for it while Fable is the model in use
    # and the fallback can actually serve Fable.
    if fable_active and fable_spent:
        blocker = fable_switch_blocker(target)
        if blocker is None:
            return leave(
                f"{SOURCE_EMAIL} Fable weekly budget {fable_detail} "
                f"-- switched to {TARGET_EMAIL}.",
                f"Fable {fable_detail}",
                "fable",
                # A live refusal means turns are failing right now, so this is
                # as urgent as a usageStatus wall -- don't sit out the backstop.
                urgent=fable_walled,
            )
        notify_fable_blocked(blocker, fable_detail, now)
        return f"stay-on-{SOURCE_NAME} (Fable {fable_detail} but {blocker})"

    if proj is None:
        # No usage yet, or the budget is on track to outlast the window.
        return f"stay-on-{SOURCE_NAME} (5h budget outlasts window)"
    return f"stay-on-{SOURCE_NAME} (5h empties in ~{proj:.0f}m)"


def fable_hold_reason(
    source: dict | None, fable_active: bool, now_dt: datetime
) -> str | None:
    """Why we must stay on the fallback for Fable's sake, or None.

    Without this the two budgets fight: the 5-hour signal says the primary has
    recovered, the Fable rule says it is spent, and the accounts flap once per
    check. A live reading of the primary's bucket wins whenever its usage is
    fetchable; the away-time ``pending_fable_reset_at`` covers the common case
    where the inactive account's usage reads unavailable. With neither, allow
    the return -- the away rule then re-fires once against a fresh active-account
    read and arms the value properly, instead of looping.
    """
    if not fable_active:
        return None
    if source is not None and source.get("usageStatus") == "ok":
        free = fable_free_pct(source.get("usage") or {})
        if free is not None:
            if free <= FABLE_AWAY_MIN_FREE_PCT:
                return f"{SOURCE_NAME} Fable at {free:.0f}% free"
            return None
    pending = get_pending_fable_reset()
    if pending is not None and now_dt < pending:
        return f"{SOURCE_NAME} Fable spent until {pending:%b %d %H:%M}Z"
    return None


def handle_target_active(
    swap: str, source: dict | None, now: float, fable_active: bool
) -> str:
    """On the fallback: return to the primary once its 5h window has reset,
    unless Fable is in use and the primary's Fable bucket is still spent.

    Returns a short decision string for the audit log.
    """
    now_dt = datetime.now(timezone.utc)

    # Signal 1 (robust, fetch-independent): wall clock has passed the reset time
    # we recorded when switching away. Works even when the primary's usage reads
    # "unavailable" as the inactive account.
    pending = get_pending_reset()
    away_reason = get_pending_away_reason()
    reason = None
    if away_reason == "fable" and not fable_active:
        wait = switch_too_soon(now)
        if wait is None:
            reason = "Fable no longer active after min switch interval"
        elif pending is None:
            return f"stay-on-{TARGET_NAME} (Fable inactive; min interval {wait:.0f}s)"
    if pending is not None and now_dt >= pending:
        reason = f"past recorded 5h reset {pending:%H:%M}Z"

    # Signal 2 (only when the inactive usage IS fetchable): 5h headroom jumped
    # back to fresh. Also read usageStatus / the weekly window to guard against
    # returning to a positively walled account. A status like "unavailable"
    # means "no current data" for the inactive account and does not block the
    # wall-clock return signal.
    weekly_walled = False
    source_status = source.get("usageStatus") if source else None
    if is_limiting_status(source_status):
        weekly_walled = True
    if source and source_status == "ok":
        usage = source.get("usage") or {}
        weekly_free = window_free_pct(usage, "sevenDay")
        weekly_walled = (
            weekly_free is not None and weekly_free <= WEEKLY_WALL_FREE_PCT
        )
        if reason is None:
            free = window_free_pct(usage, "fiveHour")
            if free is not None and free >= RETURN_FRESH_FREE_PCT:
                reason = f"5h reset observed (~{free:.0f}% free)"

    if reason is None:
        if pending is not None:
            mins = (pending - now_dt).total_seconds() / 60.0
            return f"stay-on-{TARGET_NAME} ({SOURCE_NAME} 5h resets in ~{mins:.0f}m)"
        status = source.get("usageStatus") if source else "no-source"
        return f"stay-on-{TARGET_NAME} (no reset signal; source {status})"
    if weekly_walled:
        if is_limiting_status(source_status):
            return (
                f"stay-on-{TARGET_NAME} "
                f"({SOURCE_NAME} usageStatus={source_status})"
            )
        return f"stay-on-{TARGET_NAME} ({SOURCE_NAME} weekly walled)"
    fable_hold = fable_hold_reason(source, fable_active, now_dt)
    if fable_hold is not None:
        return f"stay-on-{TARGET_NAME} ({reason} but {fable_hold})"
    note = do_switch(
        swap, SOURCE_EMAIL, now,
        f"{SOURCE_EMAIL} 5-hour window reset ({reason}) "
        f"-- switched back from {TARGET_EMAIL}.",
    )
    return f"RETURN->{SOURCE_NAME} ({reason}){note}"


def account_snapshot(accounts: list) -> str:
    """Compact one-line view of both accounts' free headroom for the log."""
    parts = []
    for a in accounts:
        email = (a.get("email") or "?").split("@")[0]
        usage = a.get("usage") or {}
        h = window_free_pct(usage, "fiveHour")
        w = window_free_pct(usage, "sevenDay")
        r = minutes_until_reset(usage, "fiveHour")
        e = project_empty_minutes(usage, "fiveHour", FIVE_HOUR_WINDOW_MINS)
        flag = "*" if a.get("active") else " "
        hs = f"{h:.0f}" if h is not None else "?"
        ws = f"{w:.0f}" if w is not None else "?"
        rs = f"{r:.0f}m" if r is not None else "?"
        es = f"{e:.0f}m" if e is not None else "-"
        # Fable's bucket, from lastGoodUsage when the account is inactive and
        # has no live usage -- marked "~" so a stale reading is visible as such.
        f_live = fable_free_pct(a.get("usage"))
        f_any = account_fable_free_pct(a)
        fs = "?" if f_any is None else f"{f_any:.0f}%{'' if f_live is not None else '~'}"
        parts.append(
            f"{flag}{email}[5h={hs}% empty_in={es} resets_in={rs} "
            f"7d={ws}% fable={fs} {a.get('usageStatus')}]"
        )
    return " ".join(parts)


def main() -> int:
    # Drain the hook payload on stdin. PostToolUse passes the full tool_response,
    # which can exceed the pipe buffer -- leaving it unread risks blocking the
    # writer. Only ``transcript_path`` is used (to tell which model this session
    # is running); the usage decision comes from claude-swap.
    raw = sys.stdin.read()
    payload = json.loads(raw) if raw.strip() else {}

    now = time.time()
    lock_file = acquire_evaluation_lock()
    if lock_file is None:
        return 0
    try:
        return evaluate(payload, now)
    finally:
        fcntl.flock(lock_file, fcntl.LOCK_UN)
        lock_file.close()


def evaluate(payload: dict, now: float) -> int:
    # Kill-switch: flip ENABLED above (permanent) or set
    # CLAUDE_AUTO_SWAP_DISABLED=1 (per-shell) to disable auto-swapping without
    # unwiring the hook from settings.json. Default is enabled.
    enabled = ENABLED and os.environ.get("CLAUDE_AUTO_SWAP_DISABLED", "0") != "1"
    # Stamp Fable usage *before* the throttle: this is the only place a session's
    # model is visible, and the session that evaluates is usually not the one
    # running Fable. Cheap (a transcript tail read, and at most one state write
    # per FABLE_STAMP_MIN_INTERVAL_SECONDS). Wall stamps are account-scoped below
    # once the active account is known.
    if enabled:
        stamp_fable_use(payload)
    # Throttle before anything else, including the log: at PostToolUse cadence
    # the un-throttled path would run per tool call and bury the audit log.
    if throttled(now):
        return 0
    # Heartbeat next, before anything can raise, so a gap in the log during
    # active work means "the hook never fired" (not "fired but crashed early").
    # The event name is recorded because which events actually fire is the
    # question behind "why didn't it switch": a request refused before it
    # reaches a model produces no tool call, so PostToolUse never runs and
    # UserPromptSubmit (which fires *before* the next request goes out) is what
    # gets the switch in.
    log(f"invoked ({payload.get('hook_event_name', '?')})")
    if not enabled:
        log("skip: auto-swap disabled")
        return 0
    swap = resolve_swap()
    if not swap:
        log("skip: claude-swap not found")
        return 0  # claude-swap not installed; nothing to do

    try:
        proc = subprocess.run(
            [swap, "--list", "--json"], capture_output=True, text=True,
            timeout=LIST_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired:
        # Same class as the failure below -- the usage API is unreachable or
        # slow. Logged, not silenced, and retried on the next check.
        log(f"skip: --list timed out after {LIST_TIMEOUT_SECONDS:.0f}s")
        return 0
    if proc.returncode != 0 or not proc.stdout.strip():
        # Usage API unreachable / not logged in: note it and skip this turn.
        err = (proc.stderr or proc.stdout or "").strip().splitlines()
        log(f"skip: --list failed rc={proc.returncode} {err[:1]}")
        return 0

    accounts = json.loads(proc.stdout).get("accounts") or []
    active = next((a for a in accounts if a.get("active")), None)
    snap = account_snapshot(accounts)
    if not active:
        log(f"{snap} :: skip (no active account)")
        return 0  # can't tell which account is active; nothing to do

    active_email = active.get("email")
    stamp_fable_use(payload, active_email if isinstance(active_email, str) else None)
    fable_walled = fable_recently_walled(now, SOURCE_EMAIL)
    fable_active = fable_recently_active(now) or fable_recently_walled(now)
    if active_email == SOURCE_EMAIL:
        # We are on the primary: any armed "return at reset" is moot. Clear it so
        # a stale value can't drive an unwanted return after a manual switch.
        clear_pending_resets()
        target = next(
            (a for a in accounts if a.get("email") == TARGET_EMAIL), None
        )
        decision = handle_source_active(
            swap, active.get("usage") or {}, active.get("usageStatus"), now,
            fable_active, fable_walled, target,
        )
    elif active_email == TARGET_EMAIL:
        source = next(
            (a for a in accounts if a.get("email") == SOURCE_EMAIL), None
        )
        decision = handle_target_active(swap, source, now, fable_active)
    else:
        decision = f"other account active ({active_email})"
    mark = "fable-walled " if fable_walled else "fable " if fable_active else ""
    log(f"{snap} :: {mark}{decision}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
