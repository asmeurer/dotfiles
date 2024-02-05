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

##
# DELUXE-USR-LOCAL-BIN-INSERT
# (do not remove this comment)
##
echo $PATH | grep -q -s "/usr/local/bin"
if [ $? -eq 1 ] ; then
    PATH=$PATH:/usr/local/bin
    export PATH
fi


# This needs to be fairly high up so that anything below it can use stuff in
# the miniconda base environment.

# Automatically activate certain conda environments when cd-ing into or out of
# the given directories
function cd () {
    builtin cd "$@"
    . <($HOME/bin/get_conda_env_for_cd.py "$@")
    export OLDPWD
}

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('~/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
        . "$HOME/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# condax
export PATH="/home/aaronmeurer/.local/bin:$PATH"

alias act="conda deactivate; conda activate"
alias deact="conda deactivate; conda activate base"
# # complete source activate. Thanks to Paul Kienzle from NIST for the
# # suggestion.
if [ -n "$MAC" ]; then
    _activate_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$(LS $HOME/anaconda/envs | tr [:upper:] [:lower:] | lam -s \" - -s \")" -- "$cur" ));
    }
else
    _activate_complete ()
    {
        local cur="${COMP_WORDS[COMP_CWORD]}";
        COMPREPLY=($(compgen -W "$("ls" $HOME/anaconda/envs | tr [:upper:] [:lower:] | paste -d \" - -d \")" -- "$cur" ));
    }
fi

complete -F _activate_complete "act"

export SUDO_PS1="\[\h:\w\] \u\$ "

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
export SYSTEMPYTHON='/Library/Frameworks/Python.framework/Versions/Current/bin/python'
export SYSTEMPYTHON_32='arch -i386 /Library/Frameworks/Python.framework/Versions/Current/bin/python'
export SYSTEMPYTHON_64='arch -x86_64 /Library/Frameworks/Python.framework/Versions/Current/bin/python'
export SYSTEMPYTHON32='/Library/Frameworks/Python.framework/Versions/3.2/bin/python3.2'
export SYSTEMPYTHON32_32='arch -i386 /Library/Frameworks/Python.framework/Versions/3.2/bin/python3.2'
export SYSTEMPYTHON32_64='arch -x86_64 /Library/Frameworks/Python.framework/Versions/3.2/bin/python3.2'
export SYSTEMPYTHON27='/Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7'
export SYSTEMPYTHON27_32='arch -i386 /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7'
export SYSTEMPYTHON27_64='arch -x86_64 /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7'
export SYSTEMPYTHON26='/Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6'
export SYSTEMPYTHON26_32='arch -i386 /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6'
export SYSTEMPYTHON26_64='arch -x86_64 /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6'
export SYSTEMPYTHON25='/Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5'
export SYSTEMPYTHON25_32='arch -i386 /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5'
export SYSTEMPYTHON25_64='arch -x86_64 /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5'
export SYSTEMPYTHON24='/Library/Frameworks/Python.framework/Versions/2.4/bin/python2.4'

alias systempython="$SYSTEMPYTHON"
alias systempython-32="$SYSTEMPYTHON_32"
alias systempython-64="$SYSTEMPYTHON_64"
alias systempython3.2="$SYSTEMPYTHON32"
alias systempython3.2-32="$SYSTEMPYTHON32_32"
alias systempython3.2-64="$SYSTEMPYTHON32_64"
alias systempython2.7="$SYSTEMPYTHON27"
alias systempython2.7-32="$SYSTEMPYTHON27_32"
alias systempython2.7-64="$SYSTEMPYTHON27_64"
alias systempython2.6="$SYSTEMPYTHON26"
alias systempython2.6-32="$SYSTEMPYTHON26_32"
alias systempython2.6-64="$SYSTEMPYTHON26_64"
alias systempython2.5="$SYSTEMPYTHON25"
alias systempython2.5-32="$SYSTEMPYTHON25_32"
alias systempython2.5-64="$SYSTEMPYTHON25_64"
alias systempython2.4="$SYSTEMPYTHON24"

# Ignore duplicate entries in the command history
HISTCONTROL=ignoredups:erasedups


# Set a separate history file per tty
export TTY=$(basename `tty`)
mkdir -p ~/.bash_history_files/
export HISTFILE="$HOME/.bash_history_files/${TTY}_history"
# Always append to the history file
shopt -s histappend
# Save the history after every command, instead of just when the shell exits
export PROMPT_COMMAND="history -a"
# Save timestamps to the history file
export HISTTIMEFORMAT="%F %T "

# Enable iTerm2 shell integration. This needs to go below the above code that
# sets PROMPT_COMMAND
if [ -n "$MAC" ]; then
    source $HOME/.iterm2_shell_integration.bash
fi

# Prevents overriding files with >.  Use >! to override.
set -o noclobber
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

# Make commands of the same name resume a stopped job instead of starting a
# new process when one exists. Useful if I accidentally suspend emacs and
# forget about it.
export auto_resume=exact

if [ -n "$MAC" ]; then
    alias ls='ls -AGFlha'
else
    alias ls='ls --color -AFlha'
fi

# Make l do ls when the input is a directory and less when it is a file
l() {
    local arg
    for arg; do
        if [[ ! $arg =~ ^- ]]; then
            if [[ -d $arg ]]; then
                ls "$@"
                return
            elif [[ -f $arg ]]; then
                less "$@"
                return
            else
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

alias sl=ls
# dc is a real command, but it's just some useless calculator
alias dc=cd

# List processes running under Rosetta. https://apple.stackexchange.com/a/431166/6446
alias rosettaprocesses="ps -p `fuser /usr/libexec/rosetta/runtime 2> /dev/null`"

PATH=$PATH:/usr/local/texlive/2017/bin/x86_64-darwin
# test -r /opt/sw/bin/init.sh && . /opt/sw/bin/init.sh
unset MANPATH

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

alias doctestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/doctest sympy/polys/'

alias polytestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/test sympy/polys/'

alias oldalltestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test'

alias alltestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test'

alias alldoctestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 ./bin/doctest; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 ./bin/doctest; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 ./bin/doctest'

alias testrisch='./bin/doctest sympy/integrals/risch.py; ./bin/test sympy/integrals/tests/test_risch.py sympy/integrals/tests/test_rde.py sympy/integrals/tests/test_prde.py sympy/integrals/tests/test_cds.py'

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

# Can't actually get metal yet
# (http://code.google.com/p/iterm2/issues/detail?id=1904), so we just use a
# similar shade of gray
TAB_GRAY="\033]6;1;bg;red;brightness;211\a\033]6;1;bg;green;brightness;211\a\033]6;1;bg;blue;brightness;211\a"

# Potential new colors
TAB_OLIVE="\033]6;1;bg;red;brightness;128\a\033]6;1;bg;green;brightness;128\a\033]6;1;bg;blue;brightness;0\a"
TAB_LIME="\033]6;1;bg;red;brightness;191\a\033]6;1;bg;green;brightness;255\a\033]6;1;bg;blue;brightness;0\a"
TAB_GOLD="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;green;brightness;223\a\033]6;1;bg;blue;brightness;0\a"

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
PATH=$PATH:/usr/local/Trolltech/Qt-4.4.3/bin
PATH=$PATH:/Library/Frameworks/Maple.framework/Versions/Current/bin
PATH=$PATH:/usr/texbin
PATH=$PATH:$HOME/Documents/git-hg/bin
PATH=$PATH:$HOME/Documents/depot_tools # For building Chromium
export LSCOLORS=eAfAcAdAbAegedabagacad
export CLICOLOR_FORCE=1 # Always use colors with ls, even when piping to less
export EMACSCLIENT="emacsclient -a 'emacs-server-start' -nw --socket-name=emacs-$TTY"
export EDITOR="$EMACSCLIENT"
# Make less search case insensitive, always use raw input mode (to show
# colors), don't ring the bell incessantly
export LESS='-RIq'
export PYTHONSTARTUP=$HOME/.pythonrc.py

alias wine='/Applications/Darwine/Wine.bundle/Contents/bin/wine'
alias e="$EMACSCLIENT"
alias m=mypython
alias my=mypython
alias mypy=mypython
alias killemacs='pkill -SIGUSR2 emacs'
alias homework='cd "$DIR_HOMEWORK"'
alias fantasia='/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Commands/java -jar /Applications/Fantasia.jar'
if [ -n "$MAC" ]; then
    alias free="osascript ~/Library/Scripts/Free\ Space.applescript"
else
    alias free='df -h /'
fi
alias rehash='hash -r'
alias cdpwd='cd `pwd`'

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

# Setting PATH for Python 2.7
# The orginal version is saved in .profile.pysave
PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/2.7/bin"

# Setting PATH for MacPython 2.6
# The orginal version is saved in .profile.pysave
PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/2.6/bin"


# Setting PATH for MacPython 2.5
# The orginal version is saved in .profile.pysave
PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/2.5/bin"

PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/3.0/bin"

# Setting PATH for Python 3.2
# The orginal version is saved in .profile.pysave
PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/3.2/bin"
export PATH

# Sage
PATH="${PATH}:/Applications/Sage-5.2-OSX-64bit-10.6.app/Contents/Resources/sage"
export PATH

# Use the git version of emacs
PATH="$HOME/Documents/emacs/src:$PATH"
PATH="$HOME/Documents/emacs/lib-src:$PATH"

# Don't use the git version of emacs
PATH="$HOME/anaconda/envs/emacs/bin:$PATH"

PATH="$PATH:$HOME/Documents/cask/bin"

# GPGTools (put before /usr/local/bin/)
PATH="/usr/local/MacGPG2/bin:$PATH"

# SSH Signing (with Secretive)
if [ -n  "$MAC" ]; then
    SSH_AUTH_SOCK="$HOME/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh"
fi

# Haskell
PATH="$HOME/Library/Haskell/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"

# Go
PATH="$PATH:/usr/local/go/bin"

# Hunspell
PATH="$HOME/anaconda/envs/hunspell/bin:$PATH"

# Custom scripts
PATH="$HOME/bin/:$PATH"

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

conda-build-all() {
    for CONDA_PY in 26 27 33 34; do
        export CONDA_PY
        conda build $@
    done
}

conda-remove-test() {
    rm -rf ~/anaconda/envs/test
}

export PATH

export PATH=`~/uniqpath`

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

# This line needs to stay at the bottom of the file.
source ~/Documents/git/contrib/completion/git-completion.bash
# Complete all git subcommands and flags
export GIT_COMPLETION_SHOW_ALL_COMMANDS=1
export GIT_COMPLETION_SHOW_ALL=1
export GIT_PROMPT_FILE=~/Documents/git/contrib/completion/git-prompt.sh
source $GIT_PROMPT_FILE

if [ -z "$MAC" ]; then
    eval "$(maestral completion bash)"
fi

export GPG_TTY=$(tty)

hash -r

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/Downloads/google-cloud-sdk/path.bash.inc" ]; then source "$HOME/Downloads/google-cloud-sdk/path.bash.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/Downloads/google-cloud-sdk/completion.bash.inc" ]; then source "$HOME/Downloads/google-cloud-sdk/completion.bash.inc"; fi

# Apply automatic environment activation to new tabs. At the end so that
# miniconda3/bin is put at the front of the PATH.
act base
cd .

# Created by `pipx` on 2024-01-09 08:17:31
export PATH="$PATH:/Users/aaronmeurer/.local/bin"
# Shell-GPT integration BASH v0.1
_sgpt_bash() {
if [[ -n "$READLINE_LINE" ]]; then
    # Remove any leading # and space characters
    READLINE_LINE=${READLINE_LINE##\#*([[:space:]])}
    history -s "# $READLINE_LINE"
	READLINE_LINE=$(sgpt --shell <<< "$READLINE_LINE")
    READLINE_POINT=${#READLINE_LINE}
fi
}
bind -x '"\C-x\C-o": _sgpt_bash'
# Shell-GPT integration BASH v0.1

source /Users/aaronmeurer/.bash_completions/condax.sh
