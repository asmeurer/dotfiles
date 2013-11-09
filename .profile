echo
fortune
echo


##
# DELUXE-USR-LOCAL-BIN-INSERT
# (do not remove this comment)
##
echo $PATH | grep -q -s "/usr/local/bin"
if [ $? -eq 1 ] ; then
    PATH=$PATH:/usr/local/bin
    export PATH
fi

export SUDO_PS1="\[\h:\w\] \u\\$ "

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

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

alias ls='ls -AGFlha --color'

test -r /sw/bin/init.sh && . /sw/bin/init.sh

alias grep='grep -i --color=always'
alias mkdir='mkdir -p'
alias cds='cd ~/Documents/sympy'
alias cdd='cd ~/Documents'
alias pudb='python -m pudb.run'

alias open='xdg-open'

export PS1='\[\e[1;37;40m\]\W\[\e[1;36;40m\]$(__git_ps1 "%s")\[\e[1;31;40m\]\$\[\e[0m\]'


# Date PS1
#export PS1='\[\e[1;31;40m\]\h:\[\e[0m\]\[\e[1;34;40m\]\W\[\e[0m\]\[\e[1;31;40m\] \u\[\e[1;30;40m\]`date "+%Y"`\[\e[0m\]\[\e[1;37;40m\]`date "+%m"`\[\e[0m\]\[\e[1;30;40m\]`date "+%d"`\[\e[0m\]\[\e[1;37;40m\]`date "+%H"`\[\e[0m\]\[\e[1;30;40m\]`date "+%M"`\[\e[0m\]\[\e[1;37;40m\]`date "+%S"`\[\e[0m\]\[\e[0;33m\]$(__git_ps1 "(%s)")\[\e[31;40m\]\$\[\e[0m\]'
# old PS1
#export PS1="\[\e[31;40m\]\h:\W \u$\[\e[0m\]$"
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
export LSCOLORS=eAfAcAdAbAegedabagacad
export CLICOLOR_FORCE=1 # Always use colors with ls, even when piping to less
export EDITOR='emacs'
export LESS='-RI' # Make less search case insensitive, and always use raw input mode (to show colors)
export PYTHONSTARTUP=$HOME/.pythonrc.py

# Use the git version of emacs
PATH="/home/asmeurer/Documents/emacs/src:$PATH"
export PATH

alias e=emacs

eval "`pip completion --bash`"

# added by Miniconda3 2.0.3 installer
export PATH="/home/asmeurer/anaconda/bin:$PATH"

# This line needs to stay at the bottom of the file.
source ~/.git-completion.bash
