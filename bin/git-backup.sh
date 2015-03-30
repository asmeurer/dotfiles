#!/bin/sh
# This commits any unstaged changes in a git repository, then
# immideiatly reverts them back to being unstaged.  The idea is that
# this will backup the changes so that they can be recovered via `git
# reflog` in case they are accidently removed via `git reset --hard`.

git stash save --keep-index "Backup Commit (WIP) `date "+%Y-%m-%d %H:%M:%S %a"`"
git stash pop --index
