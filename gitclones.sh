#/usr/bin/env bash

mkdir -p ~/Documents/gists/
cd ~/Documents/gists/
git clone git@gist.github.com:3187620.git gist-3187620

cd ~/Documents/
git clone git@github.com:github/hub.git
git clone git@github.com:git/git.git
git clone git@github.com:ipython/ipython.git
git clone git@github.com:mirrors/emacs.git
git clone git@github.com:inducer/pudb.git

# Needed in .emacs:
# TODO: byte compile some stuff
# git clone git://git.sv.gnu.org/auctex.git
# cd auctex
# ./autogen.sh
git clone git@github.com:dacap/keyfreq.git
git clone git@github.com:cask/cask.git
cd ~
cask
cd ~/Documents/
git clone git@github.com:winterTTr/ace-jump-mode.git
git clone git@github.com:nonsequitur/smex.git
git clone git@github.com:technomancy/ido-ubiquitous.git
git clone git://jblevins.org/git/markdown-mode.git
fink --yes install bzr-py27
bzr branch lp:mediawiki-el
git clone git@github.com:yoshiki/yaml-mode.git
git clone git@github.com:defunkt/coffee-mode.git
git clone git@github.com:fgallina/python.el.git
cd python.el
git remote add github git@github.com:asmeurer/python.el.git
git fetch github
git checkout indentation
cd ~/Documents/
git clone git@github.com:purcell/mmm-mode.git
# TODO: doctest-mode
git clone git@github.com:auto-complete/auto-complete.git
git clone git@github.com:auto-complete/popup-el.git
git clone git@github.com:auto-complete/fuzzy-el.git
git clone git@github.com:kiwanami/emacs-deferred.git
git clone git@github.com:kiwanami/emacs-ctable.git
git clone git@github.com:kiwanami/emacs-epc.git
git clone git@github.com:tkf/emacs-jedi.git
cd emacs-jedi
conda create -p env jedi epc
cd ~/Documents/
git clone git@github.com:m2ym/popwin-el.git
git clone git@github.com:m2ym/direx-el.git
git clone git@github.com:tkf/emacs-jedi-direx.git
git clone git@github.com:m2ym/yascroll-el.git
git clone git@github.com:magnars/expand-region.el.git
git clone git@github.com:magnars/multiple-cursors.el.git
git clone http://www.dr-qubit.org/git/undo-tree.git
git clone git@github.com:kiwanami/emacs-window-layout.git
git clone git@github.com:kiwanami/emacs-window-manager.git
git clone git@github.com:gempesaw/ido-vertical-mode.el.git

mkdir -p ~/Documents/Continuum
cd ~/Documents/Continuum
git clone git@github.com:pydata/conda.git
git clone git@github.com:pydata/conda-recipes.git
