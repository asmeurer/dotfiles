#/usr/bin/env bash
set -xe

clone-or-pull () {
    dir=$(python -c "
print('$2' or '$1'.rsplit('.git', 1)[0].split('/')[-1])
")
    if [[ -d $dir ]]
    then
        cd $dir
        git pull
        cd ..
    else
        git clone $1
    fi
}

mkdir -p ~/Documents/gists/
cd ~/Documents/gists/
clone-or-pull git@gist.github.com:3187620.git gist-3187620

cd ~/Documents/
clone-or-pull git@github.com:github/hub.git
clone-or-pull git@github.com:git/git.git
clone-or-pull git@github.com:ipython/ipython.git
clone-or-pull git@github.com:mirrors/emacs.git
clone-or-pull git@github.com:inducer/pudb.git

# Needed in .emacs:
# TODO: byte compile some stuff
# clone-or-pull git://git.sv.gnu.org/auctex.git
# cd auctex
# ./autogen.sh
clone-or-pull git@github.com:dacap/keyfreq.git
clone-or-pull git@github.com:cask/cask.git
cd ~
cask
cask update
cd ~/Documents/
clone-or-pull git@github.com:winterTTr/ace-jump-mode.git
clone-or-pull git@github.com:nonsequitur/smex.git
# Until https://github.com/nonsequitur/smex/pull/12 is merged
cd smex
git remote add haxney git@github.com:haxney/smex.git
git fetch haxney
git checkout haxney/customize
cd ~/Documents/
clone-or-pull git@github.com:technomancy/ido-ubiquitous.git
clone-or-pull git://jblevins.org/git/markdown-mode.git
clone-or-pull git@github.com:yoshiki/yaml-mode.git
clone-or-pull git@github.com:defunkt/coffee-mode.git
clone-or-pull git@github.com:fgallina/python.el.git
cd python.el
git remote add github git@github.com:asmeurer/python.el.git
git fetch github
git checkout indentation
cd ~/Documents/
clone-or-pull git@github.com:purcell/mmm-mode.git
# TODO: doctest-mode
clone-or-pull git@github.com:auto-complete/auto-complete.git
clone-or-pull git@github.com:auto-complete/popup-el.git
clone-or-pull git@github.com:auto-complete/fuzzy-el.git
clone-or-pull git@github.com:kiwanami/emacs-deferred.git
clone-or-pull git@github.com:kiwanami/emacs-ctable.git
clone-or-pull git@github.com:kiwanami/emacs-epc.git
clone-or-pull git@github.com:tkf/emacs-jedi.git
clone-or-pull git@github.com:tkf/emacs-python-environment.git
cd emacs-jedi
conda create -p env jedi epc
cd ~/Documents/
clone-or-pull git@github.com:m2ym/popwin-el.git
clone-or-pull git@github.com:m2ym/direx-el.git
clone-or-pull git@github.com:tkf/emacs-jedi-direx.git
clone-or-pull git@github.com:m2ym/yascroll-el.git
clone-or-pull git@github.com:magnars/expand-region.el.git
clone-or-pull git@github.com:magnars/multiple-cursors.el.git
clone-or-pull http://www.dr-qubit.org/git/undo-tree.git
clone-or-pull git@github.com:kiwanami/emacs-window-layout.git
clone-or-pull git@github.com:kiwanami/emacs-window-manager.git
clone-or-pull git@github.com:gempesaw/ido-vertical-mode.el.git
clone-or-pull git@github.com:joddie/pcre2el.git
clone-or-pull git@github.com:syohex/emacs-anzu.git
clone-or-pull git@github.com:emacsmirror/xterm-frobs.git
clone-or-pull git@github.com:emacsmirror/xterm-title.git
clone-or-pull git@github.com:mhayashi1120/Emacs-langtool.git
clone-or-pull git@github.com:benma/visual-regexp.el.git
clone-or-pull git@github.com:benma/visual-regexp-steroids.el.git
clone-or-pull git@github.com:juergenhoetzel/profile-dotemacs.git
clone-or-pull git@github.com:nex3/sass-mode.git
clone-or-pull git@github.com:nex3/haml-mode.git
clone-or-pull git@github.com:Bruce-Connor/aggressive-indent-mode.git
clone-or-pull git@github.com:Bruce-Connor/names.git

mkdir -p ~/Documents/Continuum
cd ~/Documents/Continuum
clone-or-pull git@github.com:pydata/conda.git
clone-or-pull git@github.com:pydata/conda-recipes.git
