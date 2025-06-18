if [[ $(uname) == "Darwin" ]]; then
    MAC=1
fi

if [ -z "$MAC" ]; then
    echo
    fortune
    echo
fi

# Don't waste time doing mail checking
unset MAILCHECK

# Use italics supported terminfo
/usr/bin/tic ~/Documents/gists/gist-3187620/xterm-256color-italic.terminfo
export TERM=xterm-256color-italic
alias ssh='TERM=xterm-256color ssh'
alias vagrant='TERM=xterm-256color vagrant'

function addtopath {
    case ":$PATH:" in
        *":$1:"*) :;; # already there
        *) PATH="$1:$PATH";; # or PATH="$PATH:$1"
    esac
}

addtopath "$HOME/miniconda3/bin"

# Automatically activate certain conda environments when cd-ing into or out of
# the given directories
function cd () {
    builtin cd "$@"
    . <($HOME/bin/get_conda_env_for_cd.py "$@")
    export OLDPWD
}

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/aaronmeurer/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/aaronmeurer/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/aaronmeurer/miniconda3/etc/profile.d/conda.sh"
    else
        addtopath "/Users/aaronmeurer/miniconda3/bin"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# condax
addtopath "/home/aaronmeurer/.local/bin"

alias act="conda deactivate; conda activate"
alias deact="conda deactivate; conda activate base"
# # complete source activate. Thanks to Paul Kienzle from NIST for the
# # suggestion.
if [ -n "$MAC" ]; then
    _activate_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$(LS $HOME/miniconda3/envs | tr [:upper:] [:lower:] | lam -s \" - -s \")" -- "$cur" ));
    }
else
    _activate_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$("ls" $HOME/miniconda3/envs | tr [:upper:] [:lower:] | paste -d \" - -d \")" -- "$cur" ));
    }
fi

complete -F _activate_complete "act"

export SUDO_PS1="\[\h:\w\] \u\$ "

# Ignore duplicate entries in the command history
HISTCONTROL=ignoredups:erasedups


# Set a separate history file per tty
export TTY=$(basename `tty`)
mkdir -p ~/.bash_history_files/
export HISTFILE="$HOME/.bash_history_files/${TTY}_history"
# Always append to the history file
shopt -s histappend
# Save the history after every command, instead of just when the shell exits
if [ -z "$PROMPT_COMMAND" ]; then
    export PROMPT_COMMAND="history -a"
fi
# Save timestamps to the history file
export HISTTIMEFORMAT="%F %T "

# Enable iTerm2 shell integration. This needs to go below the above code that
# sets PROMPT_COMMAND. Only run if 'iterm2' is not already in PROMPT_COMMAND
if [ -n "$MAC" ] && ! echo "$PROMPT_COMMAND" | grep -q iterm2; then
    source $HOME/.iterm2_shell_integration.bash
fi

# # Prevents overriding files with >.  Use >! to override.
# set -o noclobber
# Prevents C-d from ending the Terminal session.
set -o ignoreeof

# Fixes minor spelling errors in cd pathnames.
shopt -s cdspell

# Fix minor spelling errors on word completion if the given name does not exist
# Requires bash 4 or greater
if test $BASH_VERSINFO -ge 4;
then
    shopt -s dirspell;
fi

# Complete hostnames after @
shopt -s hostcomplete

# Don't complete on empty lines (it hangs bash and is not very useful)
shopt -s no_empty_cmd_completion

# Enable more advanced globbing
shopt -s globstar
shopt -s extglob

shopt -s expand_aliases

# Make commands of the same name resume a stopped job instead of starting a
# new process when one exists. Useful if I accidentally suspend emacs and
# forget about it.
export auto_resume=exact

if [ -n "$MAC" ]; then
    alias ls='ls -AGFlha'
else
    alias ls='ls --color -AFlha'
fi


# Make l do ls when the input is a directory and less when it is a file. Pass
# json files to jq.
l() {
    local arg

    for arg; do
        if [[ ! $arg =~ ^- ]]; then
            if [[ -d $arg ]]; then
                # directory
                ls "$@"
                return
            elif [[ -f $arg ]]; then
                # file
                if [[ $arg == *.json ]]; then
                    # JSON file - check if there are multiple JSON files
                    local json_files=()
                    local all_json=true

                    for f in "$@"; do
                        if [[ ! $f =~ ^- ]]; then
                            if [[ ! -f $f || ! $f == *.json ]]; then
                                all_json=false
                                break
                            fi
                            json_files+=("$f")
                        fi
                    done

                    if [[ $all_json == true && ${#json_files[@]} -gt 1 ]]; then
                        # Multiple JSON files - create temporary files
                        local tmpdir=$(mktemp -d)
                        trap 'rm -rf "$tmpdir"' EXIT

                        for json_file in "${json_files[@]}"; do
                            local basename=$(basename "$json_file")
                            jq . -C "$json_file" > "$tmpdir/$basename"
                        done

                        # Use less to view all temporary files
                        (cd "$tmpdir" && less *)
                    else
                        # Single JSON file or mixed with non-JSON - just pass
                        # straight to jq
                        cat "$@" | jq . -C | less
                    fi
                    return
                else
                    less "$@"
                    return
                fi
            else
                # something else
                file "$@"
                return
            fi
        fi
    done
    # If no non-flag arguments were found default to ls .
    ls
}

# Typos
alias it='git'
alias tit='git'
alias bit='git'
alias fit='git'
alias gi='git'
alias gt='git'
alias gti='git'
alias got='git'
alias gut='git'
alias ggit='git'
alias gir='git'
alias hit='git'
alias giut='git'
alias gigt='git'
alias g9t='git'
alias girt='git'
alias igt='git'
alias guit='git'
alias tgit='git'
alias goit='git'
alias ghit='git'
alias bgit='git'
alias vgit='git'
alias egit='git'
alias qgit='git'
alias gig='git'
alias fgit='git'

alias sl=ls
# dc is a real command, but it's just some useless calculator
alias dc=cd

# List processes running under Rosetta. https://apple.stackexchange.com/a/431166/6446
alias rosettaprocesses="ps -p `fuser /usr/libexec/rosetta/runtime 2> /dev/null`"

PATH=$PATH:/usr/local/texlive/2017/bin/x86_64-darwin
# test -r /opt/sw/bin/init.sh && . /opt/sw/bin/init.sh
unset MANPATH

if [[ -z "${CLAUDECODE}" ]]; then
        alias rtf2latex='/usr/local/rtf2latex2e/rtf2latex2e.bin'
        alias grep='grep -i --color=always'
        alias mkdir='mkdir -p'
        alias cdsympy='cd ~/Documents/python/sympy/sympy'
        alias cds='cd ~/Documents/python/sympy/sympy'
        alias cdss='cd ~/Documents/python/sympy/sympy-scratch'
        alias cdsss='cd ~/Documents/python/sympy/sympy-scratch2'
        alias isympy='mypython -c %sympy'
        if [ -z "$MAC" ]; then
            alias top='top -o %CPU'
        else
            alias top='top -o -cpu'
        fi
        alias rm='rm -i'
        alias cp='cp -i'
        alias mv='mv -i'
fi
# Set breakpoint() in Python to call pudb
export PYTHONBREAKPOINT="pudb.set_trace"

if [ -z "$MAC" ]; then
    alias open=xdg-open
fi


cdd () {
    cd "$HOME/Documents/$@" || return
}

if [ -n "$MAC" ]; then
    _cdd_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$("ls" "$HOME/Documents/" | tr "[:upper:]" "[:lower:]" | lam -s \" - -s \")" -- "$cur" ));
    }
else
    _cdd_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$("ls" "$HOME/Documents/" | tr "[:upper:]" "[:lower:]" | paste -d \" - -d \")" -- "$cur" ));
    }
fi

complete -F _cdd_complete "cdd"

cdc () {
    cd "$HOME/Documents/Continuum/$@"
}

if [ -n "$MAC" ]; then
    _cdc_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$(LS $HOME/Documents/Continuum | tr [:upper:] [:lower:] | lam -s \" - -s \")" -- "$cur" ));
    }
else
    _cdc_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$("ls" $HOME/Documents/Continuum | tr [:upper:] [:lower:] | paste -d \" - -d \")" -- "$cur" ));
    }
fi

complete -F _cdc_complete "cdc"

# Color tabs based on directories in iTerm 2

# Colors are done as "\033]6;1;bg;red;brightness;NNN\a\033]6;1;bg;blue;brightness;NNN\a\033]6;1;bg;green;brightness;NNN\a"
# Where you have                   ^              ^                ^              ^                  ^               ^
#                                  |              |                |              |                  |               |
#                                 red------->color value          blue------>color value           green------->color value
#                                              (0-255)                         (0-255)                            (0-255)

# The easiest way to get the color codes for a color is to just type the color
# name into WolframAlpha, and it will just tell you.  Or, if you have a
# particular color on-screen that you want, you can use DigitalColor Meter in
# /Applications/Utilities/

# COLORS:
TAB_WHITE="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;255\a"
TAB_RED="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;0\a\033]6;1;bg;blue;brightness;0\a"
TAB_ORANGE="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;128\a\033]6;1;bg;blue;brightness;0\a"
TAB_GREEN="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;0\a"
TAB_PURPLE="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;0\a\033]6;1;bg;blue;brightness;255\a"
TAB_YELLOW="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;0\a"
TAB_TEAL="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;128\a\033]6;1;bg;blue;brightness;128\a"
TAB_BLUE="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;0\a\033]6;1;bg;blue;brightness;255\a"
TAB_VIOLET="\033]6;1;bg;red;brightness;170\a\033]6;1;bg;green;brightness;0\a\033]6;1;bg;blue;brightness;255\a"
TAB_BLACK="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;0\a\033]6;1;bg;blue;brightness;0\a"
TAB_PINK="\033]6;1;bg;red;brightness;214\a\033]6;1;bg;green;brightness;7\a\033]6;1;bg;blue;brightness;152n\a"
TAB_DARK_GREEN="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;50\a\033]6;1;bg;blue;brightness;32\a"
TAB_SALMON="\033]6;1;bg;red;brightness;250\a\033]6;1;bg;green;brightness;128\a\033]6;1;bg;blue;brightness;114n\a"
TAB_LIGHT_BLUE="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;255n\a"
TAB_BROWN="\033]6;1;bg;red;brightness;128\a\033]6;1;bg;green;brightness;64\a\033]6;1;bg;blue;brightness;0n\a"
TAB_LIGHT_PINK="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;153\a\033]6;1;bg;blue;brightness;255n\a"
TAB_MAROON="\033]6;1;bg;red;brightness;128\a\033]6;1;bg;green;brightness;0\a\033]6;1;bg;blue;brightness;0\a"
TAB_MINT="\033]6;1;bg;red;brightness;152\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;152\a"

# Can't actually get metal yet
# (http://code.google.com/p/iterm2/issues/detail?id=1904), so we just use a
# similar shade of gray
TAB_GRAY="\033]6;1;bg;red;brightness;211\a\033]6;1;bg;green;brightness;211\a\033]6;1;bg;blue;brightness;211\a"

# Potential new colors
TAB_OLIVE="\033]6;1;bg;red;brightness;128\a\033]6;1;bg;green;brightness;128\a\033]6;1;bg;blue;brightness;0\a"
TAB_LIME="\033]6;1;bg;red;brightness;191\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;0\a"
TAB_GOLD="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;223\a\033]6;1;bg;blue;brightness;0\a"
TAB_TAN="\033]6;1;bg;red;brightness;210\a\033]6;1;bg;green;brightness;180\a\033]6;1;bg;blue;brightness;140\a"
TAB_AQUA="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;255\a"
TAB_NAVY="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;0\a\033]6;1;bg;blue;brightness;128\a"
TAB_BEIGE="\033]6;1;bg;red;brightness;245\a\033]6;1;bg;green;brightness;245\a\033]6;1;bg;blue;brightness;220\a"
TAB_CORAL="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;127\a\033]6;1;bg;blue;brightness;80\a"

declare -A tab_colors

# Directory codes are
# sympy - Red
DIR_SYMPY="$HOME/Documents/python/sympy/sympy"
tab_colors[DIR_SYMPY]=$TAB_RED

# sympy-scratch - Orange
DIR_SYMPY_SCRATCH="$HOME/Documents/python/sympy/sympy-scratch"
tab_colors[DIR_SYMPY_SCRATCH]=$TAB_ORANGE

# sympy-bot - Purple
DIR_SYMPY_BOT="$HOME/Documents/python/sympy/sympy-bot"
tab_colors[DIR_SYMPY_BOT]=$TAB_PURPLE

# sympy other (like sympy-live or sympy.wiki) - Yellow
# Note, this one must be tested after the above ones
DIR_SYMPY_OTHER="$HOME/Documents/python/sympy"
tab_colors[DIR_SYMPY_OTHER]=$TAB_YELLOW

# conda-recipes - Purple
DIR_CONDA_RECIPES="$HOME/Documents/Continuum/conda-recipes"
tab_colors[DIR_CONDA_RECIPES]=$TAB_PURPLE

# dotfiles - Green
DIR_DOTFILES="$HOME/Documents/dotfiles"
tab_colors[DIR_DOTFILES]=$TAB_GREEN

# Blog - Teal
DIR_BLOG="$HOME/Documents/blog"
tab_colors[DIR_BLOG]=$TAB_TEAL

# homework - Blue
export DIR_HOMEWORK="$HOME/Documents/Homework/Grad/Fall 2013" # Used later by homework alias
tab_colors[DIR_MYPYTHON]=$TAB_BLUE

# mypython - Blue
DIR_MYPYTHON="$HOME/Documents/mypython"
tab_colors[DIR_HOMEWORK]=$TAB_BLUE

# work directories

# Pyflyby - Violet
DIR_PYFLYBY="$HOME/Documents/pyflyby"
tab_colors[DIR_PYFLYBY]=$TAB_VIOLET

# papyri - Purple
DIR_PAPYRI="$HOME/Documents/papyri"
tab_colors[DIR_PAPYRI]=$TAB_PURPLE

# pytorch - Maroon
DIR_PYTORCH="$HOME/Documents/pytorch"
tab_colors[DIR_PYTORCH]=$TAB_MAROON

# Old - Black
DIR_ZURICH="$HOME/Documents/zurich-full"
DIR_STRUCT_RET="$HOME/Documents/struct-ret"
DIR_DASK="$HOME/Documents/dask"
DIR_CONTINUUM="$HOME/Documents/Continuum"
DIR_CONDA="$HOME/Documents/Continuum/conda"
DIR_TRANSMUTAGEN="$HOME/Documents/transmutagen"
DIR_CONDA_BUILD="$HOME/Documents/Continuum/conda-build"
tab_colors[DIR_ZURICH]=$TAB_BLACK
tab_colors[DIR_STRUCT_RET]=$TAB_BLACK
tab_colors[DIR_DASK]=$TAB_BLACK
tab_colors[DIR_CONTINUUM]=$TAB_BLACK
tab_colors[DIR_CONDA]=$TAB_BLACK
tab_colors[DIR_TRANSMUTAGEN]=$TAB_BLACK
tab_colors[DIR_CONDA_BUILD]=$TAB_BLACK

# conda-store - Black
DIR_CONDA_STORE="$HOME/Documents/conda-store"
tab_colors[DIR_CONDA_STORE]=$TAB_BLACK

# Numba - Pink
DIR_NUMBA="$HOME/Documents/numba"
tab_colors[DIR_NUMBA]=$TAB_PINK

# Array API Tests - Pink
DIR_ARRAY_API_TESTS="$HOME/Documents/array-api-tests"
tab_colors[DIR_ARRAY_API_TESTS]=$TAB_PINK

# Array API - Light Pink
DIR_ARRAY_API="$HOME/Documents/array-api"
tab_colors[DIR_ARRAY_API]=$TAB_LIGHT_PINK

# NumPy - Brown
DIR_NUMPY="$HOME/Documents/numpy"
tab_colors[DIR_NUMPY]=$TAB_BROWN

# Array API Compat - Dark Green
DIR_ARRAY_API_COMPAT="$HOME/Documents/array-api-compat"
tab_colors[DIR_ARRAY_API_COMPAT]=$TAB_DARK_GREEN

# Array API Strict - Mint
DIR_ARRAY_API_STRICT="$HOME/Documents/array-api-strict"
tab_colors[DIR_ARRAY_API_STRICT]=$TAB_MINT

# ndindex - Salmon
DIR_NDINDEX="$HOME/Documents/ndindex"
tab_colors[DIR_NDINDEX]=$TAB_SALMON

# versioned-hdf5 light blue
DIR_VERSIONED_HDF5="$HOME/Documents/versioned-hdf5"
tab_colors[DIR_VERSIONED_HDF5]=$TAB_LIGHT_BLUE

set_tab_color () {
    FOUND='no'

    # To make this work correctly with subdirectories, put higher level
    # directories later in the list.

    for dir in "${!tab_colors[@]}";
    do
        # ${!var} is the value of the variable name in var
        SEARCH_DIR=${!dir}
        COLOR=${tab_colors[$dir]}
        if grep -q "$SEARCH_DIR/.*" <<< "$PWD/"
        then
            FOUND='yes'
            echo -n -e $COLOR
        fi
        if [[ $FOUND == 'yes' ]]
        then
            break
        fi
    done

    if [[ $FOUND == 'no' ]]
    then
        echo -n -e $TAB_GRAY
    fi
    # Clear cruft from the tab title (http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#s3)
    echo -n -e "\033]0;\007"
}

# Note, ${CONDA_DEFAULT_ENV#base} is not quite right. It removes any leading
# "base" from the environment name, but this will be wrong if there happens to
# be an environment that starts with "base". As far as I can tell, there is no
# way to replace only exact matches in parameter expansion.
export PS1='\[\e[1;30;40m\]$( [[ "${CONDA_DEFAULT_ENV}" == "base" ]] || printf "%s" ${CONDA_DEFAULT_ENV} )\[\e[1;37;40m\]\W\[\e[1;36;40m\]$(__git_ps1 "%s")\[\e[1;31;40m\]\$\[\e[0m\]\[$(set_tab_color)\]'
#export PS1='\[\e[1;37;40m\]\W\[\e[1;36;40m\]$(__git_ps1 "%s")\[\e[1;31;40m\]\$\[\e[0m\]'

# Date PS1
#export PS1='\[\e[1;31;40m\]\h:\[\e[0m\]\[\e[1;34;40m\]\W\[\e[0m\]\[\e[1;31;40m\] \u\[\e[1;30;40m\]`date "+%Y"`\[\e[0m\]\[\e[1;37;40m\]`date "+%m"`\[\e[0m\]\[\e[1;30;40m\]`date "+%d"`\[\e[0m\]\[\e[1;37;40m\]`date "+%H"`\[\e[0m\]\[\e[1;30;40m\]`date "+%M"`\[\e[0m\]\[\e[1;37;40m\]`date "+%S"`\[\e[0m\]\[\e[0;33m\]$(__git_ps1 "(%s)")\[\e[31;40m\]\$\[\e[0m\]'
# old PS1
#export PS1="\[\e[31;40m\]\h:\W \u$\[\e[0m\]$"
#GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_STATESEPARATOR=''
export LSCOLORS=eAfAcAdAbAegedabagacad
export CLICOLOR_FORCE=1 # Always use colors with ls, even when piping to less
export EMACSCLIENT="emacsclient -a 'emacs-server-start' -nw --socket-name=emacs-$TTY"
export EDITOR="$EMACSCLIENT"
# Make less search case insensitive, always use raw input mode (to show
# colors), don't ring the bell incessantly
export LESS='-RIq'
export PYTHONSTARTUP=$HOME/.pythonrc.py

alias e="$EMACSCLIENT"
alias m=mypython
alias my=mypython
alias mypy=mypython
alias killemacs='pkill -SIGUSR2 -l emacs'
alias kill9emacs="pkill -9 -f -l emacs-$TTY"
alias homework='cd "$DIR_HOMEWORK"'
alias fantasia='/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Commands/java -jar /Applications/Fantasia.jar'
if [ -n "$MAC" ]; then
    alias free="osascript ~/Library/Scripts/Free\ Space.applescript"
else
    alias free='df -h /'
fi
alias rehash='hash -r'
alias cdpwd='cd `pwd`'
alias ollama-pull-all='for model in $(ollama list | awk '\''{print $1}'\''); do echo "Pulling $model"; ollama pull $model; done'
alias ollama-list-by-size='(ollama list | head -n1 && ollama list | tail -n +2 | awk -F "[[:space:]]+" '\''{
    size_num = $3
    size_unit = $4
    if (size_unit == "GB") {
        size_mb = size_num * 1024
    } else if (size_unit == "MB") {
        size_mb = size_num
    } else {
        size_mb = 0
    }
    print size_mb "\t" $0
}'\'' | sort -n | cut -f2-)'
alias claude="/Users/aaronmeurer/.claude/local/claude"

# Compare JSON files with proper formatting
diff-json() {
    if [[ $# -ne 2 ]]; then
        echo "Usage: diff-json file1.json file2.json"
        return 1
    fi

    local tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT

    jq . "$1" > "$tmpdir/file1.json" || { echo "Error: Failed to parse $1 as JSON"; return 1; }
    jq . "$2" > "$tmpdir/file2.json" || { echo "Error: Failed to parse $2 as JSON"; return 1; }

    git diff --no-index "$tmpdir/file1.json" "$tmpdir/file2.json"
}


sizeup () {
    if [[ -z "$1" ]]; then
        DIR=.
    else
        DIR="$1"
    fi
    du -a -h -x "$DIR" | sort -h
}

saydone () {
    ($@; say done)
}

recompile-emacs () {
    cd ~/Documents/emacs
    git reset --hard
    git clean -df
    git clean -Xdf
    git pull
    ./autogen.sh
    ./configure --without-x
    make bootstrap
    make all
    cd -
}

rebuild-numpy () (
    set -e
    trap 'echo -e "\\033[0;31mError: Rebuilding numpy failed\\033[0m"' ERR
    if [[ "$PWD" != "$HOME/Documents/numpy" ]]; then
        echo -e "\\033[0;31mError: You must be in ~/Documents/numpy to rebuild numpy\\033[0m"
        return 1
    fi

    git clean -dfX
    git clean -dfx numpy/
    git submodule update --init --recursive
    spin build -j16
    /bin/cp -Rf build-install/usr/lib/python3.*/site-packages/numpy/ numpy

    python -c 'import numpy; print("Built NumPy", numpy.__version__)'
)

# Don't use the git version of emacs
addtopath "$HOME/miniconda3/envs/emacs/bin"

# Homebrew
if [ -n "$MAC" ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# SSH Signing (with Secretive)
if [ -n  "$MAC" ]; then
    SSH_AUTH_SOCK="$HOME/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh"
fi

# Custom scripts
addtopath "$HOME/bin/"

# Store huggingface models on external drive (unset this if the drive is not
# present)
export HF_HUB_CACHE=/Volumes/Crucial/huggingface/hub

# This is the output of 'register-python-argcomplete conda'. We use this
# instead of
#
# eval "$(register-python-argcomplete conda)"
#
# for performance.

_python_argcomplete() {
    local IFS=$'\013'
    local SUPPRESS_SPACE=0
    if compopt +o nospace 2> /dev/null; then
        SUPPRESS_SPACE=1
    fi
    COMPREPLY=( $(IFS="$IFS" \
                     COMP_LINE="$COMP_LINE" \
                     COMP_POINT="$COMP_POINT" \
                     COMP_TYPE="$COMP_TYPE" \
                     _ARGCOMPLETE_COMP_WORDBREAKS="$COMP_WORDBREAKS" \
                     _ARGCOMPLETE=1 \
                     _ARGCOMPLETE_SUPPRESS_SPACE=$SUPPRESS_SPACE \
                     "$1" 8>&1 9>&2 1>/dev/null 2>/dev/null) )
    if [[ $? != 0 ]]; then
        unset COMPREPLY
    elif [[ $SUPPRESS_SPACE == 1 ]] && [[ "$COMPREPLY" =~ [=/:]$ ]]; then
        compopt -o nospace
    fi
}
complete -o nospace -o default -F _python_argcomplete "conda"

# END output of 'register-python-argcomplete conda'


. $HOME/.bash_completion.d/python-argcomplete.sh

conda-remove-test() {
    rm -rf ~/miniconda3/envs/test
}

alias pp='pbpaste >'

# export PYTHONPATH=${PYTHONPATH}:/sw/lib/qt4-x11/lib/python2.6/site-packages:/usr/local/lib/python
MKL_NUM_THREADS=1
export MKL_NUM_THREADS

# The output of 'pip completion --bash'. Copied here instead of using
#
# eval "`pip completion --bash`"
#
# for performance reasons.

# pip bash completion start
_pip_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                             COMP_CWORD=$COMP_CWORD \
                             PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip
# pip bash completion end

# https://github.com/fabric/fabric/issues/6#issuecomment-15182638

_fab_completion() {
    COMPREPLY=()

    local cur="${COMP_WORDS[COMP_CWORD]}"

    local tasks=$(fab --shortlist 2>/dev/null)
    COMPREPLY=( $(compgen -W "${tasks}" -- ${cur}) )
}

complete -F _fab_completion fab

export GPG_TTY=$(tty)

hash -r

# Apply automatic environment activation to new tabs. At the end so that
# miniconda3/bin is put at the front of the PATH.
deact
cd .

# Created by `pipx` on 2024-01-09 08:17:31
export PATH="$PATH:/Users/aaronmeurer/.local/bin"
# Shell-GPT integration BASH v0.1
_sgpt_bash() {
    if [[ -n "$READLINE_LINE" ]]; then
        # Remove any leading # and space characters
        READLINE_LINE=${READLINE_LINE##\#*([[:space:]])}
        history -s "# $READLINE_LINE"
        READLINE_LINE=$(sgpt --no-interaction --no-cache --shell <<< "$READLINE_LINE")
        READLINE_POINT=${#READLINE_LINE}
    fi
}
if [[ -z "${CLAUDECODE}" ]]; then
    bind -x '"\C-x\C-o": _sgpt_bash'
fi
# Shell-GPT integration BASH v0.1

source /Users/aaronmeurer/.bash_completions/condax.sh

addtopath "/Users/aaronmeurer/.pixi/bin"

addtopath "/Users/aaronmeurer/.cache/lm-studio/bin"

# This line needs to stay at the bottom of the file.
source ~/Documents/git/contrib/completion/git-completion.bash
# Complete all git subcommands and flags
export GIT_COMPLETION_SHOW_ALL_COMMANDS=1
export GIT_COMPLETION_SHOW_ALL=1
export GIT_PROMPT_FILE=~/Documents/git/contrib/completion/git-prompt.sh
source $GIT_PROMPT_FILE

# deduplicate PATH entries
export PATH=$(echo "$PATH" | tr ':' '\n' | awk '!seen[$0]++' | tr '\n' ':')
