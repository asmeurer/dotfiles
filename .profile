echo
/sw/bin/fortune
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
export TIGCC="/usr/local/tigcc"
export PATH="${PATH}:${TIGCC}/bin"
export SUDO_PS1="\[\h:\w\] \u\\$ "

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias python=/sw/bin/python
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

alias ls='ls -AG@Flha'

test -r /sw/bin/init.sh && . /sw/bin/init.sh

alias rtf2latex='/usr/local/rtf2latex2e/rtf2latex2e.bin'
alias grep='grep -i --color=always'
alias mkdir='mkdir -p'
alias cdsympy='cd ~/Documents/python/sympy/sympy'
alias cds='cd ~/Documents/python/sympy/sympy'
alias cdss='cd ~/Documents/python/sympy/sympy-scratch'
alias cdd='cd ~/Documents'
alias pudb='python -m pudb.run'
alias doctestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/doctest sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/doctest sympy/polys/'

alias polytestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 bin/test sympy/polys/; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 bin/test sympy/polys/'

alias oldalltestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test'

alias alltestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 setup.py test; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 setup.py test'

alias alldoctestall='SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /sw/bin/python2.5 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=gmpy /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 ./bin/doctest; SYMPY_GROUND_TYPES=python /sw/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=python /sw/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=python /sw/bin/python2.5 ./bin/doctest; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7 ./bin/doctest; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.6/bin/python2.6 ./bin/doctest; SYMPY_GROUND_TYPES=python /Library/Frameworks/Python.framework/Versions/2.5/bin/python2.5 ./bin/doctest'

alias testrisch='./bin/doctest sympy/integrals/risch.py; ./bin/test sympy/integrals/tests/test_risch.py sympy/integrals/tests/test_rde.py sympy/integrals/tests/test_prde.py'

export PS1='\[\e[1;37;40m\]\W\[\e[1;36;40m\]$(__git_ps1 "%s")\[\e[1;31;40m\]\$\[\e[0m\]'


# Date PS1
#export PS1='\[\e[1;31;40m\]\h:\[\e[0m\]\[\e[1;34;40m\]\W\[\e[0m\]\[\e[1;31;40m\] \u\[\e[1;30;40m\]`date "+%Y"`\[\e[0m\]\[\e[1;37;40m\]`date "+%m"`\[\e[0m\]\[\e[1;30;40m\]`date "+%d"`\[\e[0m\]\[\e[1;37;40m\]`date "+%H"`\[\e[0m\]\[\e[1;30;40m\]`date "+%M"`\[\e[0m\]\[\e[1;37;40m\]`date "+%S"`\[\e[0m\]\[\e[0;33m\]$(__git_ps1 "(%s)")\[\e[31;40m\]\$\[\e[0m\]'
# old PS1
#export PS1="\[\e[31;40m\]\h:\W \u$\[\e[0m\]$"
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
PATH=$PATH:/usr/local/Trolltech/Qt-4.4.3/bin
PATH=$PATH:/Library/Frameworks/Maple.framework/Versions/Current/bin
PATH=$PATH:/usr/texbin
PATH=$PATH:/Users/aaronmeurer/Documents/git-hg/bin
PATH=$PATH:/Users/aaronmeurer/Documents/depot_tools # For building Chromium
export LSCOLORS=eAfAcAdAbAegedabagacad
export CLICOLOR_FORCE=1 # Always use colors with ls, even when piping to less
export EDITOR='emacs'
export LESS='-RI' # Make less search case insensitive, and always use raw input mode (to show colors)
export PYTHONSTARTUP=$HOME/.pythonrc.py

alias wine='/Applications/Darwine/Wine.bundle/Contents/bin/wine'
alias emacs='/usr/bin/emacs'
alias homework='cd ~/Documents/Homework/Fall\ 2011/'
alias fantasia='/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Commands/java -jar /Applications/Fantasia.jar'


# Setting PATH for Python 2.7
# The orginal version is saved in .profile.pysave
PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/2.7/bin"

# Setting PATH for MacPython 2.6
# The orginal version is saved in .profile.pysave
PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/2.6/bin"


# Setting PATH for MacPython 2.5
# The orginal version is saved in .profile.pysave
PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/2.5/bin"

PATH="${PATH}:/Library/Frameworks/Python.framework/Versions/3.0/bin/"

# Setting PATH for Python 3.2
# The orginal version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.2/bin:${PATH}"
export PATH

PATH="${PATH}:/Applications/sage"
export PATH

export PATH=`/usr/local/bin/uniqpath`

export PYTHONPATH=${PYTHONPATH}:/sw/lib/qt4-x11/lib/python2.6/site-packages:/usr/local/lib/python
MKL_NUM_THREADS=1
export MKL_NUM_THREADS

eval "`pip completion --bash`"

# This line needs to stay at the bottom of the file.
source ~/.git-completion.bash
