#/usr/bin/env bash
set -e

clone-or-pull () {
    dir=$(python -c "
print('$2' or '$1'.rsplit('.git', 1)[0].split('/')[-1])
")
    if [[ -d $dir ]]
    then
        echo "Pulling $dir"
        cd $dir
        git pull
        cd ..
    else
        echo "Cloning $dir"
        git clone $1 $2
    fi
}

mkdir -p ~/Documents/gists/
cd ~/Documents/gists/
clone-or-pull git@gist.github.com:3187620.git gist-3187620

cd ~/Documents/
clone-or-pull git@github.com:github/hub.git
cd hub
./script/build
cp bin/hub ~/bin/
cd ~/Documents

clone-or-pull git@github.com:git/git.git
cd ~/Documents/git/contrib/diff-highlight/
make
cd ~/Documents

clone-or-pull git@github.com:ipython/ipython.git
clone-or-pull git://git.sv.gnu.org/emacs.git
clone-or-pull git@github.com:inducer/pudb.git

# Needed in .emacs:
# TODO: byte compile some stuff
# clone-or-pull git://git.sv.gnu.org/auctex.git
# cd auctex
# ./autogen.sh

clone-or-pull git@github.com:jwiegley/use-package.git
cd ~/Documents/use-package
make
cd ~

clone-or-pull git@github.com:dacap/keyfreq.git
clone-or-pull git@github.com:cask/cask.git
cd ~
echo "Running cask"
cask
echo "Running cask update"
cask update
cd ~/Documents/
clone-or-pull git@github.com:winterTTr/ace-jump-mode.git
clone-or-pull git@github.com:nonsequitur/smex.git
# Until https://github.com/nonsequitur/smex/pull/12 is merged
cd smex
# ignore remote already exists.
git remote add haxney git@github.com:haxney/smex.git || true
git fetch haxney
git checkout customize
git branch --set-upstream-to=haxney/customize customize
cd ~/Documents/
clone-or-pull git@github.com:technomancy/ido-ubiquitous.git
clone-or-pull git://jblevins.org/git/markdown-mode.git
clone-or-pull git@github.com:yoshiki/yaml-mode.git
clone-or-pull git@github.com:defunkt/coffee-mode.git
clone-or-pull git@github.com:fgallina/python.el.git
cd python.el
# ignore remote already exists
git remote add github git@github.com:asmeurer/python.el.git || true
git fetch github
git checkout indentation
# ignore remote
git branch --set-upstream-to=github/indentation indentation
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
echo "Creating conda environment for jedi"
conda install -m -p env jedi epc
cd ~/Documents/
clone-or-pull git@github.com:m2ym/popwin-el.git
clone-or-pull git@github.com:m2ym/direx-el.git
clone-or-pull git@github.com:tkf/emacs-jedi-direx.git
clone-or-pull git@github.com:m2ym/yascroll-el.git
clone-or-pull git@github.com:magnars/expand-region.el.git
clone-or-pull git@github.com:magnars/multiple-cursors.el.git
# clone-or-pull http://www.dr-qubit.org/git/undo-tree.git
clone-or-pull git@github.com:emacsmirror/undo-tree.git
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
clone-or-pull git@github.com:emacsmirror/isearch-plus.git

clone-or-pull git@github.com:asmeurer/mypython
ln -s -f ~/Documents/mypython/bin/mypython ~/bin/mypython

conda install \
      catimg fortune emacs argcomplete prefsync hunspell-en pyflakes \
      mpmath ipython conda-build anaconda xonsh

activate-global-python-argcomplete --user

echo "Running prefsync"
prefsync ~/Library/Preferences/org.pqrs.Karabiner.plist ~/Documents/dotfiles/Library/Preferences/org.pqrs.Karabiner.plist

conda install -m -n fortune fortune
