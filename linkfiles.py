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

TODO:

- Support linking directories
- Warn when linking a file not tracked by git

"""

from __future__ import print_function

import sys
import glob
import argparse

from os import walk, symlink, makedirs
from os.path import (join, relpath, abspath, exists, lexists, expanduser,
                     split, islink)

def fullpath(path):
    return abspath(expanduser(path))

from itertools import chain

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

    for dirpath, dirnames, filenames in walk(args.source):
        dest_head = join(args.destination, relpath(dirpath, start=args.source))
        for file in filenames:
            source = join(dirpath, file)
            if any(abspath(source).startswith(i) for i in ignore):
                continue
            else:
                destination = join(dest_head, relpath(join(dirpath, file),
                    dirpath))
                if not lexists(fullpath(destination)):
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
                        "already exists and is not a symbolic link")

if __name__ == '__main__':
    sys.exit(main())
