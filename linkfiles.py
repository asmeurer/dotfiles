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

"""

import sys
import os
import glob
import argparse

def main():
    parser = argparse.ArgumentParser(description="""Link the dotfiles to where
    they're supposed to go.""",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('-s/--source', action='store', default='.', help="""The
    source of the files to link""")
    parser.add_argument('-d/--destination', action='store', help="""The
    destination of the files to link""", default='~')
    parser.add_argument("-i/--ignorefile", action='store', help="""File containing
    list of paths to ignore (the file itself and this script are always
    ignored)""", default="IGNORE")

    args = parser.parse_args()

    if os.path.exists(args.ignorefile):
        with open(args.ignorefile) as f:
            ignore = f.read().strip().split()
    else:
        ignore = []
        print("No ignore file found")

    ignore.append(__file__)

    for dirpath, dirnames, filenames in os.walk(args.source):
        pass


if __name__ == '__main__':
    sys.exit(main())
