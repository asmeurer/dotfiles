echo
~/anaconda/envs/fortune/bin/fortune
echo

# Don't waste time doing mail checking
unset MAILCHECK

# Use italics supported terminfo
tic ~/Documents/gists/gist-3187620/xterm-256color-italic.terminfo
export TERM=xterm-256color-italic
alias ssh='TERM=xterm-256color ssh'
alias vagrant='TERM=xterm-256color vagrant'

COMPUTER=`scutil --get ComputerName`

##
# DELUXE-USR-LOCAL-BIN-INSERT
# (do not remove this comment)
##
echo $PATH | grep -q -s "/usr/local/bin"
if [ $? -eq 1 ] ; then
    PATH=$PATH:/usr/local/bin
    export PATH
fi

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


# Prevents overriding files with >.  Use >! to override.
set -o noclobber
# Prevents C-d from ending the Terminal session.
set -o ignoreeof

# Fixes minor spelling errors in cd pathnames.
shopt -s cdspell

# Fix minor spelling errors on word completion if the given name does not exist
# Requires bash 4 or greater
if test $BASH_VERSINFO -ge 4;
    then shopt -s dirspell;
fi

# Complete hostnames after @
shopt -s hostcomplete

# Don't complete on empty lines (it hangs bash and is not very useful)
shopt -s no_empty_cmd_completion

# Make commands of the same name resume a stopped job instead of starting a
# new process when one exists. Useful if I accidentally suspend emacs and
# forget about it.
export auto_resume=1

alias ls='ls -AG@Flha'
alias  l='ls -AG@Flha'

# Typos
alias it='git'
alias sl=ls

test -r /sw/bin/init.sh && . /sw/bin/init.sh

alias rtf2latex='/usr/local/rtf2latex2e/rtf2latex2e.bin'
alias grep='grep -i --color=always'
alias mkdir='mkdir -p'
alias cdsympy='cd ~/Documents/python/sympy/sympy'
alias cds='cd ~/Documents/python/sympy/sympy'
alias cdss='cd ~/Documents/python/sympy/sympy-scratch'
alias cdsss='cd ~/Documents/python/sympy/sympy-scratch2'

cdd () {
    cd "/Users/aaronmeurer/Documents/$@"
}

_cdd_complete ()
{
    local cur="${COMP_WORDS[COMP_CWORD]}";
    COMPREPLY=($(compgen -W "`cd $HOME/Documents/ && LS -d *`" -- "$cur" ));
}

complete -F _cdd_complete "cdd"

cdc () {
    cd "/Users/aaronmeurer/Documents/Continuum/$@"
}

_cdc_complete ()
{
    local cur="${COMP_WORDS[COMP_CWORD]}";
    COMPREPLY=($(compgen -W "`cd $HOME/Documents/Continuum/ && LS -d *`" -- "$cur" ));
}

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

# Directory codes are
# sympy - Red
DIR_SYMPY='/Users/aaronmeurer/Documents/python/sympy/sympy'
TAB_RED="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;blue;brightness;0\a\033]6;1;bg;green;brightness;0\a"
# sympy-scratch - Orange
DIR_SYMPY_SCRATCH='/Users/aaronmeurer/Documents/python/sympy/sympy-scratch'
TAB_ORANGE="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;blue;brightness;0\a\033]6;1;bg;green;brightness;128\a"
# sympy-bot - Purple
DIR_SYMPY_BOT='/Users/aaronmeurer/Documents/python/sympy/sympy-bot'
TAB_PURPLE="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;blue;brightness;255\a\033]6;1;bg;green;brightness;0\a"
# sympy other (like sympy-live or sympy.wiki) - Yellow
# Note, this one must be tested after the above ones
DIR_SYMPY_OTHER='/Users/aaronmeurer/Documents/python/sympy'
TAB_YELLOW="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;blue;brightness;0\a\033]6;1;bg;green;brightness;255\a"
# dotfiles - Green
DIR_DOTFILES='/Users/aaronmeurer/Documents/dotfiles'
TAB_GREEN="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;blue;brightness;0\a\033]6;1;bg;green;brightness;255\a"
# Continuum - Black
DIR_CONTINUUM='/Users/aaronmeurer/Documents/Continuum'
TAB_BLACK="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;blue;brightness;0\a\033]6;1;bg;green;brightness;0\a"
# Conda - White
DIR_CONDA='/Users/aaronmeurer/Documents/Continuum/conda'
TAB_WHITE="\033]6;1;bg;red;brightness;255\a\033]6;1;bg;blue;brightness;255\a\033]6;1;bg;green;brightness;255\a"
# Conda - White
DIR_CONDA_BUILD='/Users/aaronmeurer/Documents/Continuum/conda-build'
export TAB_TEAL="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;blue;brightness;128\a\033]6;1;bg;green;brightness;128\a"
# homework - Blue
export DIR_HOMEWORK="/Users/aaronmeurer/Documents/Homework/Grad/Fall 2013" # Used later by homework alias
TAB_BLUE="\033]6;1;bg;red;brightness;0\a\033]6;1;bg;blue;brightness;255\a\033]6;1;bg;green;brightness;0\a"
export DIR_CONDA_RECIPES="/Users/aaronmeurer/Documents/Continuum/conda-recipes"
# Other - default (metal)
# Can't actually get metal yet
# (http://code.google.com/p/iterm2/issues/detail?id=1904), so we just use a
# similar shade of gray
TAB_GRAY="\033]6;1;bg;red;brightness;110\a\033]6;1;bg;blue;brightness;110\a\033]6;1;bg;green;brightness;110\a"

set_tab_color () {
    FOUND='no'

    # To make this work correctly with subdirectories, put higher level
    # directories later in the list.

    # TODO: Is there a better way to do this?
    # Yes, using associative arrays
    for dir_tab in '$DIR_SYMPY $TAB_RED' '$DIR_SYMPY_SCRATCH $TAB_ORANGE' '$DIR_SYMPY_BOT $TAB_PURPLE' '$DIR_SYMPY_OTHER $TAB_YELLOW' '$DIR_DOTFILES $TAB_GREEN' '$DIR_HOMEWORK $TAB_BLUE' '$DIR_CONDA $TAB_WHITE' '$DIR_CONDA_BUILD $TAB_TEAL' '$DIR_CONDA_RECIPES $TAB_PURPLE' '$DIR_CONTINUUM $TAB_BLACK'
    do
        set -- $dir_tab
        # Dereference the variable name
        # We do things this way because set won't handle directories with
        # spaces correctly, no matter what we do.
        SEARCH_DIR=`eval echo $1`
        COLOR=`eval echo $2`
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

underline_exit () {
    es=$?
    # See
    # https://wiki.archlinux.org/index.php/Color_Bash_Prompt#List_of_colors_for_prompt_and_Bash
    # This won't actually show up as red because the prompt colors take over :(
    undred='\e[4;31m' # Red
    if test $es
    then
       echo -n -e $undred
    fi
}

export PS1='\[\r\]\[\e[1;30;40m\]$CONDA_DEFAULT_ENV\[\e[1;37;40m\]\W\[\e[1;36;40m\]$(__git_ps1 "%s")\[\e[1;31;40m\]\$\[\e[0m\]\[$(set_tab_color)\]'
#export PS1='\[\e[1;37;40m\]\W\[\e[1;36;40m\]$(__git_ps1 "%s")\[\e[1;31;40m\]\$\[\e[0m\]'


# Date PS1
#export PS1='\[\e[1;31;40m\]\h:\[\e[0m\]\[\e[1;34;40m\]\W\[\e[0m\]\[\e[1;31;40m\] \u\[\e[1;30;40m\]`date "+%Y"`\[\e[0m\]\[\e[1;37;40m\]`date "+%m"`\[\e[0m\]\[\e[1;30;40m\]`date "+%d"`\[\e[0m\]\[\e[1;37;40m\]`date "+%H"`\[\e[0m\]\[\e[1;30;40m\]`date "+%M"`\[\e[0m\]\[\e[1;37;40m\]`date "+%S"`\[\e[0m\]\[\e[0;33m\]$(__git_ps1 "(%s)")\[\e[31;40m\]\$\[\e[0m\]'
# old PS1
#export PS1="\[\e[31;40m\]\h:\W \u$\[\e[0m\]$"
#GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
GIT_PS1_STATESEPARATOR=''
PATH=$PATH:/usr/local/Trolltech/Qt-4.4.3/bin
PATH=$PATH:/Library/Frameworks/Maple.framework/Versions/Current/bin
PATH=$PATH:/usr/texbin
PATH=$PATH:/Users/aaronmeurer/Documents/git-hg/bin
PATH=$PATH:/Users/aaronmeurer/Documents/depot_tools # For building Chromium
export LSCOLORS=eAfAcAdAbAegedabagacad
export CLICOLOR_FORCE=1 # Always use colors with ls, even when piping to less
export TTY=$(basename `tty`)
export EMACSCLIENT="emacsclient -a 'emacs-server-start' -nw --socket-name=$TTY"
export EDITOR="$EMACSCLIENT"
export LESS='-RIC' # Make less search case insensitive, always use raw input
# mode (to show colors), and never scroll output
export PYTHONSTARTUP=$HOME/.pythonrc.py

alias wine='/Applications/Darwine/Wine.bundle/Contents/bin/wine'
alias emacsclient="$EMACSCLIENT"
alias e=emacsclient
alias homework='cd "$DIR_HOMEWORK"'
alias fantasia='/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Commands/java -jar /Applications/Fantasia.jar'
alias free='diskutil info / | GREP -i Free'
alias rehash='hash -r'
alias cdpwd='cd `pwd`'

sizeup () {
    if [[ -z "$1" ]]; then
        DIR=.
    else
        DIR="$1"
    fi
    du -a -h -x "$DIR" | gsort -h
}

saydone () {
    ($@; say done)
}

hub_dir="/Users/aaronmeurer/Documents/hub"
PATH="$PATH:$hub_dir"

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

# Alias git to hub
eval "$(hub alias -s)"

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
PATH="/Users/aaronmeurer/Documents/emacs/src:$PATH"
PATH="/Users/aaronmeurer/Documents/emacs/lib-src:$PATH"

# Don't use the git version of emacs
PATH="/Users/aaronmeurer/anaconda/envs/emacs/bin:$PATH"

PATH="$PATH:/Users/aaronmeurer/Documents/cask/bin"

# Haskell
PATH="$HOME/Library/Haskell/bin:$PATH"

# Go
PATH="$PATH:/usr/local/go/bin"

# Anaconda
if [[ $COMPUTER == "Aaron’s Retina MacBook Pro" ]]; then
    # Hunspell
    PATH="$HOME/anaconda3/envs/hunspell/bin:$PATH"
    PATH="$HOME/anaconda3/bin:$PATH"
else
    # Hunspell
    PATH="$HOME/anaconda/envs/hunspell/bin:$PATH"
    PATH="$HOME/anaconda/bin:$PATH"
fi


eval "$(register-python-argcomplete conda)"
eval "$(register-python-argcomplete conda-build)"
. /Users/aaronmeurer/.bash_completion.d/python-argcomplete.sh


alias act="source activate"
alias deact="source deactivate"
# # complete source activate. Thanks to Paul Kienzle from NIST for the
# # suggestion.
_activate_complete ()
{
    local cur="${COMP_WORDS[COMP_CWORD]}";
    if [[ $COMPUTER == "Aaron’s Retina MacBook Pro" ]]; then
        COMPREPLY=($(compgen -W "`cd $HOME/anaconda3/envs && LS -d *`" -- "$cur" ));
    else
        COMPREPLY=($(compgen -W "`cd $HOME/anaconda/envs && LS -d *`" -- "$cur" ));
    fi
}

complete -F _activate_complete "act"

source /Users/aaronmeurer/Documents/Continuum/conda/conda/conda-bash.sh

conda-build-all() {
    for CONDA_PY in 26 27 33 34; do
        export CONDA_PY
        conda build $@
    done
}

if [[ $COMPUTER == "Aaron’s Retina MacBook Pro" ]]; then
    conda-remove-test() {
        rm -rf ~/anaconda3/envs/test
    }
else
    conda-remove-test() {
        rm -rf ~/anaconda/envs/test
    }
fi


export PATH

export PATH=`~/uniqpath`

# export PYTHONPATH=${PYTHONPATH}:/sw/lib/qt4-x11/lib/python2.6/site-packages:/usr/local/lib/python
MKL_NUM_THREADS=1
export MKL_NUM_THREADS

eval "`pip completion --bash`"
if [[ $COMPUTER == "Aaron’s Retina MacBook Pro" ]]; then
    eval "`~/anaconda3/envs/blog-nikola/bin/nikola tabcompletion`"
fi

source ~/Documents/ipython/examples/core/ipython-completion.bash

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
source ~/Documents/git/contrib/completion/git-prompt.sh
source ~/Documents/hub/etc/hub.bash_completion.sh

source ~/.iterm_shell_integration.sh

hash -r
