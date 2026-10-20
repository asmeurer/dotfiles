These are some of my .files

Files in the root of the directory are symbolically linked to
corresponding files in ~/

Note, to symbolically link a file, you must do ``ln -s
~/Documents/dotfiles/.file ~/.file``, i.e., the paths must be
**absolute** paths (use of the ``~`` shortcut is OK).  See
http://superuser.com/questions/302312/how-to-properly-store-dotfiles-in-a-centralized-git-repository.

Files in other/ are for various other things.

# Linking

First, you have to install Anaconda, and Fink, and git (to even get this in
the first place).

Run

    ./linkfiles.py

and then

    ./gitclones.sh

For Linux, some files that go in the root directory (`/`) are in `root/`.
They aren't linked automatically yet.

# iTerm2

`iterm2/` holds the iTerm2 preferences (`com.googlecode.iterm2.plist`). It is
not symlinked; instead iTerm2 is pointed at the folder directly ("Load
settings from a custom folder") and writes changes back to it, so they can be
committed. `gitclones.sh` configures this on macOS; restart iTerm2 after
running it.
