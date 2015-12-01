#!/bin/sh
# This commits any unstaged changes in a git repository, then immediately
# reverts them back to being unstaged.  The idea is that this will backup the
# changes so that they can be recovered via `git fsck --unreachable` in case
# they are accidently removed via `git reset --hard`.

# You can find backup commits with

# git fsck --unreachable | grep commit | cut -d" " -f3 | xargs git log # --merges --no-walk --grep='Backup Commit'

# It is useful to add this as an alias in ~/.gitconfig, like

# [alias]
#     backup = !git-backup.sh
#     backups = !git fsck --unreachable | grep commit | cut -d' ' -f3 | xargs git log --merges --no-walk --grep='Backup Commit' --stat

git stash save --keep-index "Backup Commit (WIP) `date "+%Y-%m-%d %H:%M:%S %a"`"
git stash pop --index
