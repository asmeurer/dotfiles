#!/usr/bin/env python

"""
Link the dotfiles to where they're supposed to go

Usage:

$ ./linkfiles.py # Defaults to linking files from . to ~
$ ./linkfiles.py -s /path/to/source -d /path/to/destination
$ ./linkfiles.py -i IGNOREFILE

By default, a file called IGNORE in the current directory is used as the
ignore file, if it exists.  The ignorefile and this script itself are always
ignored.  The ignore file uses globs. Files and paths starting with a . will
not be glob matched unless there is an explicit . at the beginning of the
glob. An example IGNORE file might look like:

.git
.DS_Store
*~
.*~
other

Some top-level directories are linked under a different name, see RENAMES
below. In particular, Claude Code's global config lives in ~/.claude, but is
stored in this repo as claude/ (no leading dot). If it were stored as
.claude/, Claude Code would also pick it up as *project* settings whenever it
is run inside this repo, and on an unlinked machine the hooks it references
under ~/.claude/hooks would not exist yet, which blocks every tool call.

TODO:

- Support linking directories
- Warn when linking a file not tracked by git

"""

from __future__ import print_function

import sys
import glob
import argparse
from itertools import chain
from shutil import copy2

from os import walk, symlink, makedirs, remove
from os.path import (join, relpath, abspath, exists, lexists, expanduser,
                     split, islink, isfile)


def fullpath(path):
    return abspath(expanduser(path))


COPY_FILES = [
    ".config/karabiner/karabiner.json",
]

# Top-level directories in the repo that are linked under a different name in
# the destination. Keys are the name in the repo, values are the name in the
# destination (both relative to the source/destination roots).
RENAMES = {
    "claude": ".claude",
}


def rename_path(path):
    """
    Apply RENAMES to the first component of a path relative to the source.
    """
    if path == '.':
        return path
    parts = path.split('/')
    if parts[0] in RENAMES:
        parts[0] = RENAMES[parts[0]]
    return join(*parts)


def copy_file(source, destination, dry_run=False):
    if not isfile(fullpath(source)):
        return

    if not lexists(fullpath(destination)):
        if dry_run:
            print("Would copy:", end=' ')
        else:
            print("Copying:", end=' ')
        print(fullpath(source), "to", fullpath(destination))

        if not dry_run:
            dir = split(fullpath(destination))[0]
            if not exists(dir):
                makedirs(dir, exist_ok=True)
            copy2(fullpath(source), fullpath(destination))
    elif islink(fullpath(destination)):
        print("Warning:", fullpath(destination),
            "already exists and is a symbolic link", file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(description="""Link the dotfiles to where
    they're supposed to go.""",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('-s', '--source', action='store', default='.',
        help="""The source of the files to link""")
    parser.add_argument('-d', '--destination', action='store', help="""The
        destination of the files to link""", default='~')
    parser.add_argument("-i", "--ignorefile", action='store', help="""File
        containing list of paths to ignore (the file itself and this script
        are always ignored)""", default="IGNORE")
    parser.add_argument('--dry-run', action='store_true', default=False,
        help="""Show what would be done without doing it.""")

    args = parser.parse_args()

    if exists(args.ignorefile):
        with open(args.ignorefile) as f:
            ignore = f.read().strip().split()
        ignore = list(map(abspath, chain(*map(glob.glob, ignore))))
        ignore.append(abspath(args.ignorefile))
    else:
        ignore = []
        print("No ignore file found")

    ignore.append(abspath(__file__))

    for file in COPY_FILES:
        copy_file(join(args.source, file), join(args.destination, file),
            args.dry_run)

    copy_file_sources = set(abspath(join(args.source, file))
        for file in COPY_FILES)

    for dirpath, dirnames, filenames in walk(args.source):
        # Never link Python bytecode caches (they are gitignored anyway)
        dirnames[:] = [d for d in dirnames if d != '__pycache__']
        dest_head = join(args.destination,
            rename_path(relpath(dirpath, start=args.source)))
        for file in filenames:
            source = join(dirpath, file)
            if abspath(source) in copy_file_sources:
                continue
            if any(abspath(source).startswith(i) for i in ignore):
                continue
            else:
                destination = join(dest_head, relpath(join(dirpath, file),
                    dirpath))
                if islink(fullpath(destination)) and not exists(fullpath(destination)):
                    # A dangling symlink, most likely left over from a file
                    # that was moved or renamed in the repo (e.g. .claude/ ->
                    # claude/). Replace it with a link to the new location.
                    if args.dry_run:
                        print("Would relink:", end=' ')
                    else:
                        print("Relinking:", end=' ')
                    print(fullpath(source), "to", fullpath(destination),
                        "(replacing dangling symlink)")

                    if not args.dry_run:
                        remove(fullpath(destination))
                        symlink(fullpath(source), fullpath(destination))
                elif not lexists(fullpath(destination)):
                    if args.dry_run:
                        print("Would link:", end=' ')
                    else:
                        print("Linking:", end=' ')
                    print(fullpath(source), "to", fullpath(destination))

                    if not args.dry_run:
                        dir = split(fullpath(destination))[0]
                        if not exists(dir):
                            makedirs(dir, exist_ok=True)
                        symlink(fullpath(source), fullpath(destination))
                elif not islink(fullpath(destination)):
                    print("Warning:", fullpath(destination),
                        "already exists and is not a symbolic link", file=sys.stderr)

if __name__ == '__main__':
    sys.exit(main())
