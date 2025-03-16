This is a set of personal configuration files for macOS and Linux. All files
are symlinked to the home directory.

The principle files here are:

- .profile: bash configuration
- .emacs: emacs configuration
- gitclones.sh: script to install some packages
- linkfiles.py: script to symlink all files to ~
- bin/: a few custom scripts that are too complex to put as bash functions in .profile

# Code Style Guidelines
- **Python**:
  - 4-space indentation, PEP-8 compliant
  - Absolute paths preferred over relative paths
  - Never silence exceptions (prefer full tracebacks)
  - Use descriptive variable names
  - Snake_case for function names and variables
  - Document functions with docstrings

- **Shell Scripts**:
  - Use `set -e` to exit on errors
  - Include detailed comments describing functionality
  - Prefer absolute paths when possible
  - The bash configuration should be portable for both Mac and Linux. This
    includes using $HOME or ~ to reference the home directory, which is
    different on the different platforms. If something can only work on one
    platform, it should use a conditional so it isn't run on the other.

- **Security**:
  - Store API keys/tokens securely
  - Never hardcode sensitive information

- **Git Workflow**:
  - Small, focused commits with descriptive messages
