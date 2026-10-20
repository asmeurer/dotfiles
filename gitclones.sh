#!/usr/bin/env bash

# Set up a new machine, or keep an existing one up to date. The script is
# idempotent: it is safe (and intended) to re-run periodically.
#
# System packages come from Homebrew on macOS (see Brewfile) and apt on
# Linux. Python libraries live in the conda base environment; standalone
# Python CLI tools are managed with `uv tool`.
#
# Prerequisites (not installed by this script):
# - Homebrew on macOS (https://brew.sh)
# - Miniconda in ~/miniconda3 (only used for Python libraries)
# - git SSH access to GitHub

set -euo pipefail

# Steps that shouldn't abort the whole run (e.g. one repo failing to pull)
# append to this instead; a summary is printed at the end.
FAILURES=()

clone-or-pull () {
    # clone-or-pull URL [DIR]
    local url=$1
    local dir=${2:-$(basename "${url%.git}")}
    if [[ -d $dir ]]; then
        echo "Pulling $dir"
        if ! git -C "$dir" pull --ff-only; then
            FAILURES+=("git pull $dir")
            return 1
        fi
    else
        echo "Cloning $dir"
        if ! git clone "$url" "$dir"; then
            FAILURES+=("git clone $url")
            return 1
        fi
    fi
}

clone-or-pull-continue () {
    clone-or-pull "$@" || true
}

# ==== System packages ====

if [[ $(uname) == "Darwin" ]]; then
    # conda ships its own `codesign` (from cctools) that shadows
    # /usr/bin/codesign and breaks emacs-plus's post-install signing step
    # ("argument --deep was not expected"). Put the system tools first for
    # all brew commands so they find Apple's codesign, not conda's.
    local_brew() { PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH" brew "$@"; }

    local_brew update
    # Newer brew refuses to load formulas from untrusted third-party taps.
    # (`brew trust` doesn't exist in older brew, hence the || true.)
    local_brew trust d12frosted/emacs-plus 2> /dev/null || true
    local_brew trust kenn-io/tap 2> /dev/null || true
    # Don't let a single formula failure abort the git-clones / uv / dictionary
    # steps below.
    local_brew bundle --file="$HOME/Documents/dotfiles/Brewfile" \
        || FAILURES+=("brew bundle")
    # Note: this upgrades everything brew manages, not just the Brewfile
    # packages. emacs-plus has no bottles, so an emacs upgrade rebuilds from
    # source and can take a while.
    local_brew upgrade || FAILURES+=("brew upgrade")
    # emacs-plus's `emacs`/`emacsclient` shims run /Applications/Emacs.app if
    # it exists, so the emacs-plus app must live there or terminal emacs will
    # launch some other Emacs.app instead. Symlink it, but never clobber a
    # real (non-symlink) Emacs.app -- if one is in the way, warn instead of
    # silently letting it shadow the new build.
    if [[ ! -e /Applications/Emacs.app || -L /Applications/Emacs.app ]]; then
        ln -sfn "$(brew --prefix)/opt/emacs-plus@30/Emacs.app" /Applications/Emacs.app \
            || FAILURES+=("link Emacs.app into /Applications")
    else
        FAILURES+=("/Applications/Emacs.app is a real directory (not our symlink); it shadows emacs-plus. Remove it and re-run.")
    fi
else
    # Debian/Ubuntu equivalents of the Brewfile.
    sudo apt-get update
    sudo apt-get install -y \
        bash bat fd-find ripgrep \
        hunspell hunspell-en-us emacs nodejs npm golang gh
    # These aren't packaged on all releases; try them individually so one
    # missing package doesn't abort the rest.
    for pkg in duf du-dust sd git-delta procs; do
        sudo apt-get install -y "$pkg" || FAILURES+=("apt install $pkg")
    done
    # Debian installs fd and bat under different names
    mkdir -p ~/bin
    [[ -e ~/bin/fd ]] || ln -s "$(command -v fdfind)" ~/bin/fd
    [[ -e ~/bin/bat ]] || ln -s "$(command -v batcat)" ~/bin/bat
    # uv isn't in apt; use the official installer (installs to ~/.local/bin)
    if ! command -v uv > /dev/null; then
        curl -LsSf https://astral.sh/uv/install.sh | sh
        export PATH="$HOME/.local/bin:$PATH"
    fi
fi

# ==== Git clones ====

mkdir -p ~/Documents/gists/
mkdir -p ~/bin/
cd ~/Documents

if clone-or-pull git@github.com:git/git.git; then
    (cd ~/Documents/git/contrib/diff-highlight/ || exit 1; make) \
        || FAILURES+=("make diff-highlight")
fi

clone-or-pull-continue git@github.com:ipython/ipython.git
clone-or-pull-continue git@github.com:inducer/pudb.git

# Emacs packages
clone-or-pull-continue git@github.com:dacap/keyfreq.git

if clone-or-pull git@github.com:nonsequitur/smex.git; then
# Until https://github.com/nonsequitur/smex/pull/12 is merged
    (
        cd smex || exit 1
        # ignore remote already exists
        git remote add haxney git@github.com:haxney/smex.git 2> /dev/null || true
        git fetch haxney
        git checkout customize
        git branch --set-upstream-to=haxney/customize customize
    ) || FAILURES+=("smex branch setup")
fi

if clone-or-pull git@github.com:fgallina/python.el.git; then
    (
        cd python.el || exit 1
        # ignore remote already exists
        git remote add github git@github.com:asmeurer/python.el.git 2> /dev/null || true
        git fetch github
        git checkout indentation
        git branch --set-upstream-to=github/indentation indentation
    ) || FAILURES+=("python.el branch setup")
fi

clone-or-pull-continue git@github.com:purcell/mmm-mode.git
clone-or-pull-continue git@github.com:juergenhoetzel/profile-dotemacs.git

if clone-or-pull git@github.com:tkf/emacs-jedi.git; then
# .emacs runs the jedi EPC server with ~/Documents/emacs-jedi/env/bin/python,
# so the packages must be installed into that environment (a plain venv).
    (
        cd emacs-jedi || exit 1
        if [[ ! -d env ]]; then
            echo "Creating environment for emacs-jedi"
            uv venv env
        fi
        uv pip install --python env/bin/python jedi epc
    ) || FAILURES+=("emacs-jedi env setup")
fi

if clone-or-pull git@github.com:jwiegley/use-package.git; then
    (cd use-package || exit 1; make elc) || FAILURES+=("make use-package")
fi

if clone-or-pull git@github.com:asmeurer/mypython; then
    ln -sf ~/Documents/mypython/bin/mypython ~/bin/mypython
fi

if [[ $(uname) != "Darwin" ]]; then
    if clone-or-pull git@github.com:jcs/xbanish.git; then
        (cd xbanish || exit 1; make) || FAILURES+=("make xbanish")
    fi
fi

# ==== Python libraries (conda base) ====

# Python *libraries* stay in conda; standalone CLI tools are managed with uv
# below. esbonio must stay here (importable from the PATH python3) because
# .emacs runs it with "python3 -m esbonio".
CONDA_PKGS="--file=$HOME/Documents/mypython/requirements.txt argcomplete mpmath ipython conda-build matplotlib pytest sympy pudb setproctitle mamba jedi esbonio"
conda install -y -n base $CONDA_PKGS

activate-global-python-argcomplete --user

# ==== Python CLI tools (uv) ====

# Each tool gets its own isolated environment; executables land in
# ~/.local/bin. `uv tool install` is a no-op when already installed.
UV_TOOLS=(
    shell-gpt
    pyflakes              # .emacs flycheck runs the bare pyflakes executable
    pyinstrument
    jedi-language-server
    basedpyright
    xonsh
    llm
    glances
    pipdeptree
    pypinfo
    "mcp[cli]"
)
if [[ $(uname) == "Darwin" ]]; then
    # Apple Silicon only
    UV_TOOLS+=(asitop)
fi
for tool in "${UV_TOOLS[@]}"; do
    # A plain install is a no-op once uv owns the tool. On first run it can
    # collide with a leftover pipx/pixi executable of the same name; --force
    # lets uv take it over (it only overrides that guard, not real errors), so
    # retry once with --force before recording a failure.
    uv tool install "$tool" \
        || uv tool install --force "$tool" \
        || FAILURES+=("uv tool install $tool")
done
uv tool upgrade --all || FAILURES+=("uv tool upgrade --all")

# ==== hunspell dictionary ====

# Install a larger English dictionary for hunspell/Emacs.
#
# The brew/apt hunspell packages ship at most the standard en_US dictionary
# (~50k base words), which misses a lot of real English vocabulary. This is a
# SCOWL (Spell Checker Oriented Word Lists) build at size 95 (~150k base
# words), generated by the official tool at http://app.aspell.net/create. It's
# the same upstream the standard dictionary comes from, just much larger, and
# it still rejects ordinary typos.
#
# It is installed outside this repo (in ~/.local/share/hunspell) rather than
# committed here. The basename stays "en_US" so the personal word list
# ~/.hunspell_en_US still auto-loads. .emacs points hunspell at it via an
# absolute -d path; see the ispell-local-dictionary-alist there.
HUNSPELL_DICT_DIR="$HOME/.local/share/hunspell"
if [[ -f "$HUNSPELL_DICT_DIR/en_US.dic" && -f "$HUNSPELL_DICT_DIR/en_US.aff" ]]; then
    echo "SCOWL hunspell dictionary already installed; skipping download"
else
    mkdir -p "$HUNSPELL_DICT_DIR"
    echo "Downloading SCOWL size-95 en_US hunspell dictionary"
    SCOWL_TMP=$(mktemp -d)
    curl -fsSL "http://app.aspell.net/create?max_size=95&spelling=US&max_variant=3&diacritic=both&special=hacker&encoding=utf-8&format=inline&download=hunspell" -o "$SCOWL_TMP/scowl.zip"
    unzip -o -q "$SCOWL_TMP/scowl.zip" -d "$SCOWL_TMP"
    cp "$SCOWL_TMP/en_US-custom.dic" "$HUNSPELL_DICT_DIR/en_US.dic"
    cp "$SCOWL_TMP/en_US-custom.aff" "$HUNSPELL_DICT_DIR/en_US.aff"
    rm -rf "$SCOWL_TMP"
fi

# Expose the SCOWL dictionary to programs that can only pass hunspell a plain
# dictionary *name* rather than an absolute -d path (Claude Code's spellcheck
# setting, which uses "language": "en_US-large"). The conda hunspell build
# ignores DICPATH, but ~/Library/Spelling is in its built-in search path on
# macOS, so symlink the dictionary there under a distinct name (plain "en_US"
# would be shadowed by conda's own dictionary, which appears earlier in the
# search path). The ~/.hunspell_en_US-large symlink makes hunspell's automatic
# personal word list for the "en_US-large" basename resolve to the shared
# ~/.hunspell_en_US from this repo.
if [[ "$(uname)" == "Darwin" ]]; then
    mkdir -p "$HOME/Library/Spelling"
    ln -sf ../../.local/share/hunspell/en_US.aff "$HOME/Library/Spelling/en_US-large.aff"
    ln -sf ../../.local/share/hunspell/en_US.dic "$HOME/Library/Spelling/en_US-large.dic"
fi
ln -sf .hunspell_en_US "$HOME/.hunspell_en_US-large"

# ==== iTerm2 preferences ====

# Make iTerm2 load its settings from iterm2/ in this repo and write changes
# back there ("Load settings from a custom folder or URL" in Settings >
# General > Settings), so they are tracked in git. Pointing iTerm2 straight at
# the repo folder means nothing needs to be symlinked, and anything else iTerm2
# decides to write next to the plist (e.g. synced snippets) lands in the repo
# too. The three NoSync* keys select "Save changes: Automatically" (0 = when quitting,
# 1 = never, 2 = automatically; see iTermPreferenceSavePrefsMode in the iTerm2
# source). iTerm2 only reads these at launch, so restart it after changing
# them. The iterm2/ directory is listed in IGNORE so linkfiles.py skips it.
if [[ "$(uname)" == "Darwin" ]]; then
    defaults write com.googlecode.iterm2 PrefsCustomFolder -string '~/Documents/dotfiles/iterm2'
    defaults write com.googlecode.iterm2 LoadPrefsFromCustomFolder -bool true
    defaults write com.googlecode.iterm2 NoSyncNeverRemindPrefsChangesLostForFile -bool true
    defaults write com.googlecode.iterm2 NoSyncNeverRemindPrefsChangesLostForFile_selection -int 2
fi

# ==== Summary ====

if (( ${#FAILURES[@]} )); then
    echo
    echo "The following steps failed:" >&2
    printf ' - %s\n' "${FAILURES[@]}" >&2
    exit 1
fi
echo "Done."
