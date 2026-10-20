When writing Python code, avoid silencing exceptions. I'd rather see a full traceback when an error occurs.

When writing code that uses an API key or token, store this token in a secure, reusable way.

Always use American English spelling and usage (e.g. "color", "behavior", "analyze", "normalize") in all output, code, comments, and documents.

`cc-run` wrapper — how to ask me to approve a command the auto-mode classifier won't let you run. Whenever a Bash command is **blocked or denied by the auto-mode safety classifier** — whether because it's "temporarily unavailable" (can't determine safety) OR because the classifier actively **disallows** the action (e.g. it flags the command as unsafe, an "unsafe agent", a permission bypass, etc.) — do NOT give up, hand the command to me to run manually, or try to circumvent the denial. Instead, re-issue the exact same command wrapped in the `cc-run` wrapper: `/Users/aaronmeurer/.claude/bin/cc-run '<full command line as one quoted argument>'` (it runs via `bash -c`, so pipes, `&&`, `$(...)`, quotes, and redirects all work inside the single-quoted argument). `cc-run` is covered by an explicit `permissions.ask` rule in `~/.claude/settings.json`, evaluated **before** the auto-mode classifier, so it converts the hard block into a normal interactive permission prompt that I can approve. That is the whole point of the wrapper: it lets you (Claude) proactively request my approval for otherwise-disallowed commands so I don't have to run them by hand. Still exercise judgment — use it to get approval for legitimate actions the classifier is being over-cautious about, not to push through genuinely destructive or malicious operations.

GitHub-flavored markdown (gists, issue/PR comments, README files) does NOT
support `\operatorname` in math — every use renders as a MathJax error.
Replace it with `\mathrm` (or a supported macro like `\arctan`). Note that
sympy's `latex()` emits `\operatorname{atan}` etc., so always post-process
its output before pasting into GitHub markdown.
