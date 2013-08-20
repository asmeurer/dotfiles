# Configuration file for ipython.

c = get_config()

#------------------------------------------------------------------------------
# TerminalIPythonApp configuration
#------------------------------------------------------------------------------

# TerminalIPythonApp will inherit config from: BaseIPythonApplication,
# Application, InteractiveShellApp

# Execute the given command string.
# c.TerminalIPythonApp.code_to_run = ''

# The IPython profile to use.
# c.TerminalIPythonApp.profile = u'default'

# Set the log level by value or name.
# c.TerminalIPythonApp.log_level = 30

# lines of code to run at IPython startup.
# c.TerminalIPythonApp.exec_lines = []

# Enable GUI event loop integration ('qt', 'wx', 'gtk').
# c.TerminalIPythonApp.gui = None

# Pre-load matplotlib and numpy for interactive use, selecting a particular
# matplotlib backend and loop integration.
# c.TerminalIPythonApp.pylab = None

# Suppress warning messages about legacy config files
c.TerminalIPythonApp.ignore_old_config = True

# If a command or file is given via the command-line, e.g. 'ipython foo.py
# c.TerminalIPythonApp.force_interact = False

# The name of the IPython directory. This directory is used for logging
# configuration (through profiles), history storage, etc. The default is usually
# $HOME/.ipython. This options can also be specified through the environment
# variable IPYTHON_DIR.
# c.TerminalIPythonApp.ipython_dir = u'/Users/aaronmeurer/.ipython'

# Whether to display a banner upon starting IPython.
# c.TerminalIPythonApp.display_banner = True

# Start IPython quickly by skipping the loading of config files.
# c.TerminalIPythonApp.quick = False

# A list of dotted module names of IPython extensions to load.
# c.TerminalIPythonApp.extensions = []

# Whether to install the default config files into the profile dir. If a new
# profile is being created, and IPython contains config files for that profile,
# then they will be staged into the new directory.  Otherwise, default config
# files will be automatically generated.
# c.TerminalIPythonApp.copy_config_files = False

# dotted module name of an IPython extension to load.
# c.TerminalIPythonApp.extra_extension = ''

# List of files to run at IPython startup.
# c.TerminalIPythonApp.exec_files = []

# Whether to overwrite existing config files when copying
# c.TerminalIPythonApp.overwrite = False

# A file to be run
# c.TerminalIPythonApp.file_to_run = ''

#------------------------------------------------------------------------------
# TerminalIPythonApp configuration
#------------------------------------------------------------------------------

# TerminalIPythonApp will inherit config from: BaseIPythonApplication,
# Application, InteractiveShellApp

# Execute the given command string.
# c.TerminalIPythonApp.code_to_run = ''

# The IPython profile to use.
# c.TerminalIPythonApp.profile = u'default'

# Set the log level by value or name.
# c.TerminalIPythonApp.log_level = 30

# lines of code to run at IPython startup.
# c.TerminalIPythonApp.exec_lines = []

# Enable GUI event loop integration ('qt', 'wx', 'gtk').
# c.TerminalIPythonApp.gui = None

# Pre-load matplotlib and numpy for interactive use, selecting a particular
# matplotlib backend and loop integration.
# c.TerminalIPythonApp.pylab = None

# Suppress warning messages about legacy config files
# c.TerminalIPythonApp.ignore_old_config = False

# If a command or file is given via the command-line, e.g. 'ipython foo.py
# c.TerminalIPythonApp.force_interact = False

# The name of the IPython directory. This directory is used for logging
# configuration (through profiles), history storage, etc. The default is usually
# $HOME/.ipython. This options can also be specified through the environment
# variable IPYTHON_DIR.
# c.TerminalIPythonApp.ipython_dir = u'/Users/aaronmeurer/.ipython'

# Whether to display a banner upon starting IPython.
# c.TerminalIPythonApp.display_banner = True

# Start IPython quickly by skipping the loading of config files.
# c.TerminalIPythonApp.quick = False

# A list of dotted module names of IPython extensions to load.
# c.TerminalIPythonApp.extensions = []

# Whether to install the default config files into the profile dir. If a new
# profile is being created, and IPython contains config files for that profile,
# then they will be staged into the new directory.  Otherwise, default config
# files will be automatically generated.
# c.TerminalIPythonApp.copy_config_files = False

# dotted module name of an IPython extension to load.
# c.TerminalIPythonApp.extra_extension = ''

# List of files to run at IPython startup.
# c.TerminalIPythonApp.exec_files = []

# Whether to overwrite existing config files when copying
# c.TerminalIPythonApp.overwrite = False

# A file to be run
# c.TerminalIPythonApp.file_to_run = ''

#------------------------------------------------------------------------------
# InteractiveShellApp configuration
#------------------------------------------------------------------------------

# A Mixin for applications that start InteractiveShell instances.
#
# Provides configurables for loading extensions and executing files as part of
# configuring a Shell environment.
#
# Provides init_extensions() and init_code() methods, to be called after
# init_shell(), which must be implemented by subclasses.

# Execute the given command string.
# c.InteractiveShellApp.code_to_run = ''

# lines of code to run at IPython startup.
# c.InteractiveShellApp.exec_lines = []

# A list of dotted module names of IPython extensions to load.
# c.InteractiveShellApp.extensions = []

# dotted module name of an IPython extension to load.
# c.InteractiveShellApp.extra_extension = ''

# List of files to run at IPython startup.
# c.InteractiveShellApp.exec_files = []

# A file to be run
# c.InteractiveShellApp.file_to_run = ''

#------------------------------------------------------------------------------
# TerminalInteractiveShell configuration
#------------------------------------------------------------------------------

# TerminalInteractiveShell will inherit config from: InteractiveShell

# auto editing of files with syntax errors.
c.TerminalInteractiveShell.autoedit_syntax = True

# Use colors for displaying information about objects. Because this information
# is passed through a pager (like 'less'), and some pagers get confused with
# color codes, this capability can be turned off.
# c.TerminalInteractiveShell.color_info = True

#
# c.TerminalInteractiveShell.history_length = 10000

#
# c.TerminalInteractiveShell.separate_in = '\n'

# Set the color scheme (NoColor, Linux, or LightBG).
c.TerminalInteractiveShell.colors = 'LightBG'

# Autoindent IPython code entered interactively.
# c.TerminalInteractiveShell.autoindent = True

#
# c.TerminalInteractiveShell.readline_omit__names = 2

#
# c.TerminalInteractiveShell.prompt_in2 = '   .\\D.: '

#
# c.TerminalInteractiveShell.separate_out = ''

#
# c.TerminalInteractiveShell.prompt_in1 = 'In [\\#]: '

# Enable deep (recursive) reloading by default. IPython can use the deep_reload
# module which reloads changes in modules recursively (it replaces the reload()
# function, so you don't need to change anything to use it). deep_reload()
# forces a full reload of modules whose code may have changed, which the default
# reload() function does not.  When deep_reload is off, IPython will use the
# normal reload(), but deep_reload will still be available as dreload().
# c.TerminalInteractiveShell.deep_reload = False

#
# c.TerminalInteractiveShell.debug = False

# Make IPython automatically call any callable object even if you didn't type
# explicit parentheses. For example, 'str 43' becomes 'str(43)' automatically.
# The value can be '0' to disable the feature, '1' for 'smart' autocall, where
# it is not applied if there are no more arguments on the line, and '2' for
# 'full' autocall, where all callable objects are automatically called (even if
# no arguments are present). The default is '1'.
c.TerminalInteractiveShell.autocall = 0

# Number of lines of your screen, used to control printing of very long strings.
# Strings longer than this number of lines will be sent through a pager instead
# of directly printed.  The default value for this is 0, which means IPython
# will auto-detect your screen size every time it needs to print certain
# potentially long strings (this doesn't change the behavior of the 'print'
# keyword, it's only triggered internally). If for some reason this isn't
# working well (it needs curses support), specify it yourself. Otherwise don't
# change the default.
# c.TerminalInteractiveShell.screen_length = 0

# Set the editor used by IPython (default to $EDITOR/vi/notepad).
# c.TerminalInteractiveShell.editor = 'edit -w --resume'

#
# c.TerminalInteractiveShell.prompts_pad_left = True

# The part of the banner to be printed before the profile
# c.TerminalInteractiveShell.banner1 = 'Python 2.7.2 (default, Jul 31 2011, 01:52:32) \nType "copyright", "credits" or "license" for more information.\n\nIPython 0.11 -- An enhanced Interactive Python.\n?         -> Introduction and overview of IPython\'s features.\n%quickref -> Quick reference.\nhelp      -> Python\'s own help system.\nobject?   -> Details about \'object\', use \'object??\' for extra details.\n'

#
c.TerminalInteractiveShell.readline_parse_and_bind = [
    'tab: complete',
    '"\\C-l": clear-screen',
    'set show-all-if-ambiguous on',
    '"\\C-o": tab-insert',
    '"\\C-r": reverse-search-history',
    '"\\C-s": forward-search-history',
    '"\\ep": history-search-backward',
    '"\\eP": history-search-forward',
    '"\\C-p": history-search-backward',
    '"\\C-n": history-search-forward',
    '"\\C-k": kill-line',
    '"\\C-u": unix-line-discard',
    '"\\C-P": history-search-forward',
    '"\\C-d": "\\C-a\\C-k\\C-d"',
]

# The part of the banner to be printed after the profile
# c.TerminalInteractiveShell.banner2 = ''

#
# c.TerminalInteractiveShell.separate_out2 = ''

#
# c.TerminalInteractiveShell.wildcards_case_sensitive = True

#
# c.TerminalInteractiveShell.readline_merge_completions = True

# Set to confirm when you try to exit IPython with an EOF (Control-D in Unix,
# Control-Z/Enter in Windows). By typing 'exit' or 'quit', you can force a
# direct exit without any confirmation.
c.TerminalInteractiveShell.confirm_exit = False

#
# c.TerminalInteractiveShell.ipython_dir = ''

#
# c.TerminalInteractiveShell.readline_remove_delims = '-/~'

# Start logging to the default log file.
# c.TerminalInteractiveShell.logstart = False

# The name of the logfile to use.
# c.TerminalInteractiveShell.logfile = ''

# The shell program to be used for paging.
# c.TerminalInteractiveShell.pager = 'less'

# Enable magic commands to be called without the leading %.
# c.TerminalInteractiveShell.automagic = True

#
# c.TerminalInteractiveShell.readline_use = True

# Start logging to the given file in append mode.
# c.TerminalInteractiveShell.logappend = ''

#
# c.TerminalInteractiveShell.xmode = 'Context'

#
# c.TerminalInteractiveShell.quiet = False

# Enable auto setting the terminal title.
# c.TerminalInteractiveShell.term_title = False

#
# c.TerminalInteractiveShell.object_info_string_level = 0

#
# c.TerminalInteractiveShell.prompt_out = 'Out[\\#]: '

# Set the size of the output cache.  The default is 1000, you can change it
# permanently in your config file.  Setting it to 0 completely disables the
# caching system, and the minimum value accepted is 20 (if you provide a value
# less than 20, it is reset to 0 and a warning is issued).  This limit is
# defined because otherwise you'll spend more time re-flushing a too small cache
# than working
# c.TerminalInteractiveShell.cache_size = 1000

# Automatically call the pdb debugger after every exception.
# c.TerminalInteractiveShell.pdb = False

#------------------------------------------------------------------------------
# ProfileDir configuration
#------------------------------------------------------------------------------

# An object to manage the profile directory and its resources.
#
# The profile directory is used by all IPython applications, to manage
# configuration, logging and security.
#
# This object knows how to find, create and manage these directories. This
# should be used by any code that wants to handle profiles.

# Set the profile location directly. This overrides the logic used by the
# `profile` option.
# c.ProfileDir.location = u''

#------------------------------------------------------------------------------
# PlainTextFormatter configuration
#------------------------------------------------------------------------------

# The default pretty-printer.
#
# This uses :mod:`IPython.external.pretty` to compute the format data of the
# object. If the object cannot be pretty printed, :func:`repr` is used. See the
# documentation of :mod:`IPython.external.pretty` for details on how to write
# pretty printers.  Here is a simple example::
#
#     def dtype_pprinter(obj, p, cycle):
#         if cycle:
#             return p.text('dtype(...)')
#         if hasattr(obj, 'fields'):
#             if obj.fields is None:
#                 p.text(repr(obj))
#             else:
#                 p.begin_group(7, 'dtype([')
#                 for i, field in enumerate(obj.descr):
#                     if i > 0:
#                         p.text(',')
#                         p.breakable()
#                     p.pretty(field)
#                 p.end_group(7, '])')

# PlainTextFormatter will inherit config from: BaseFormatter

#
# c.PlainTextFormatter.type_printers = {}

#
# c.PlainTextFormatter.newline = '\n'

#
# c.PlainTextFormatter.float_precision = ''

#
# c.PlainTextFormatter.verbose = False

#
# c.PlainTextFormatter.deferred_printers = {}

#
# c.PlainTextFormatter.pprint = True

#
# c.PlainTextFormatter.max_width = 79

#
# c.PlainTextFormatter.singleton_printers = {}

# try:
#     import pudb.ipython
# except ImportError:
#     pass

# Only complete __all__ names on import

c.IPCompleter.limit_to__all__ = True
