#! /bin/sh
# This commits any unstaged changes in a git repository, then
# immideiatly reverts them back to being unstaged.  The idea is that
# this will backup the changes so that they can be recovered via `git
# reflog` in case they are accidently removed via `git reset --hard`.
# Note that this will move changes from the staging area (`git add`)
# back into uncommited changes.  This also means that new files added
# with `git add` without any commited changes will become untracked
# files.
git commit -a -m "Backup Commit (WIP) `date "+%Y-%m-%d %H:%M:%S %a"`" --edit
git reset HEAD^
