# Homebrew packages for macOS. Installed/updated by gitclones.sh via
#
#     brew bundle --file="$HOME/Documents/dotfiles/Brewfile"
#
# Python CLI tools are NOT listed here; they are managed with `uv tool` in
# gitclones.sh. Python libraries stay in conda (see gitclones.sh).

tap "d12frosted/emacs-plus"
tap "kenn-io/tap"   # roborev

# Emacs. emacs-plus builds from source (no bottles), so installs/upgrades
# occasionally take a while. It provides both Emacs.app (symlinked into
# /Applications by gitclones.sh) and the terminal emacs/emacsclient binaries
# on PATH. Native compilation is enabled by default in emacs-plus@30.
brew "d12frosted/emacs-plus/emacs-plus@30"

# Shell. Note: the terminal profile should point at /opt/homebrew/bin/bash.
brew "bash"
brew "coreutils"   # provides gtimeout for bounded statusline helpers

# CLI tools (moved here from pixi global)
brew "bat"
brew "bottom"      # installs `btm`
brew "duf"
brew "dust"
brew "eslint"
brew "fd"
brew "gh"          # replaces the deprecated `hub`
brew "git-delta"   # installs `delta`
brew "go"
brew "gtop"
brew "moor"        # renamed upstream from moar; still installs a `moar` alias
brew "procs"
brew "ripgrep"
brew "sd"

# Emacs support tools. hunspell ships no dictionaries; the SCOWL en_US
# dictionary is downloaded to ~/.local/share/hunspell by gitclones.sh.
brew "hunspell"
brew "node"        # for copilot.el
brew "ruff"

# Manages the Python CLI tools (see gitclones.sh)
brew "uv"

# ==== File / search / navigation ====
brew "eza"                 # modern ls replacement
brew "lsd"                 # ls replacement with icons
brew "ack"                 # grep for programmers
brew "the_silver_searcher" # `ag` code search
brew "jaq"                 # fast jq clone
brew "taplo"               # TOML toolkit
brew "mmv"                 # mass move/rename
brew "jdupes"              # duplicate file finder
brew "entr"                # run commands when files change
brew "fswatch"             # directory change monitor
brew "watchman"            # file watching service
brew "less"                # newer than the system pager

# ==== Dev tools ====
brew "git"                 # also built from source in gitclones.sh for diff-highlight
brew "tree-sitter"         # incremental parsing (Emacs uses it)
brew "pandoc"              # markup format converter
brew "shfmt"               # shell formatter
brew "hadolint"            # Dockerfile linter
brew "swiftlint"           # Swift linter
brew "xcodegen"            # generate Xcode projects
brew "chroma"              # syntax highlighter
brew "cloc"                # count lines of code
brew "expect"              # automate interactive apps
brew "qcachegrind"         # profiler visualizer

# ==== macOS-specific ====
brew "cliclick"            # emulate mouse/keyboard events
brew "spoof-mac"           # spoof MAC address
brew "macmon"              # sudoless perf monitor (Apple Silicon)
brew "mactop"              # Apple Silicon top
brew "imessage-exporter"   # export iMessage database

# ==== Media / misc ====
brew "ffmpeg"              # generate/play Bluetooth keepalive tones
brew "switchaudio-osx"     # read/switch macOS audio devices
brew "yt-dlp"              # audio/video downloader
brew "zbar"                # barcode reader
brew "gping"               # ping with a graph

# ==== AI / LLM CLI tools ====
brew "ccusage"             # Claude Code usage analyzer
brew "llama.cpp"           # local LLM inference
brew "slackdump"           # export Slack data
brew "kenn-io/tap/roborev" # AI code-review daemon

# ==== Casks ====
cask "karabiner-elements"  # keyboard customizer (.config/karabiner depends on it)
cask "font-hack-nerd-font"
cask "codex"               # OpenAI coding agent
cask "codexbar"            # menu-bar usage monitor for Codex/Claude
# Markdown viewer: MDV (https://mowglii.com/mdv/) is the default handler for
# .md/.markdown. It has no cask, so it is installed manually by dragging
# MDV.app to /Applications; the handler is set with
#     duti -s com.mowglii.MDV net.daringfireball.markdown all
