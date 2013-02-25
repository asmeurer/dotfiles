from __future__ import print_function

import sys

from IPython.core.magic import register_line_magic

ip = get_ipython()

if sys.version_info[0] > 2:
    @register_line_magic
    def print(line):
        __builtin__.print(ip.ev(line))
