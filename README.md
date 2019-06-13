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
