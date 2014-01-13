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

"""

from __future__ import print_function

import sys
import glob
import argparse

from os import walk, symlink
from os.path import join, relpath, abspath, exists

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
    else:
        ignore = []
        print("No ignore file found")

    ignore.append(__file__)

    for dirpath, dirnames, filenames in walk(args.source):
        dest_head = join(args.destination, relpath(dirpath, start=args.source))
        for file in filenames:
            fullpath = join(dirpath, file)
            if any(abspath(fullpath).startswith(i) for i in ignore):
                continue
            else:
                destination = join(dest_head, relpath(join(dirpath, file),
                    dirpath))
                if args.dry_run:
                    print("Would link:", end=' ')
                else:
                    print("Linking:", end=' ')
                print(abspath(fullpath), "to", destination)

                if not args.dry_run:
                    symlink(abspath(fullpath), destination)



if __name__ == '__main__':
    sys.exit(main())
