#!/usr/bin/env python


import sys
import os

this_exe = sys.argv[0]

def canon(p):
    return os.path.realpath(os.path.expanduser(p)) + os.path.sep

def exit(m):
    sys.exit(this_exe, m)

HOME = os.environ.get('HOME')
if not HOME:
    exit("Could not get $HOME")
HOME = canon(HOME)

PWD = os.environ.get("PWD")
if not PWD:
    exit("Could not get $PWD")
PWD = canon(PWD)

dirnames = dict(
    DIR_ARRAY_API_TESTS="$HOME/Documents/array-api-tests",
    DIR_ARRAY_API="$HOME/Documents/array-api",
    DIR_SCIPY_2023="$HOME/Documents/scipy-2023-presentation",
    DIR_NUMPY="$HOME/Documents/numpy",
    DIR_ARRAY_API_COMPAT="$HOME/Documents/array-api-compat",
    DIR_NDINDEX="$HOME/Documents/ndindex",
    DIR_VERSIONED_HDF5="$HOME/Documents/versioned-hdf5",
    DIR_PYFLYBY="$HOME/Documents/pyflyby",
    DIR_BLOG="$HOME/Documents/blog",
    DIR_SCIPY="$HOME/Documents/scipy",
    DIR_PAPYRI="$HOME/Documents/papyri",
    DIR_CONDA_STORE_SERVER="$HOME/Documents/conda-store/conda-store-server",
    DIR_PYTORCH="$HOME/Documents/pytorch",
)

for d in dirnames:
    dirnames[d] = canon(dirnames[d].replace("$HOME", HOME))

conda_envs = {}

conda_envs[dirnames['DIR_ARRAY_API']] = 'array-apis'
conda_envs[dirnames['DIR_ARRAY_API_TESTS']] = 'array-apis'
conda_envs[dirnames['DIR_ARRAY_API_COMPAT']] = 'array-apis'
conda_envs[dirnames['DIR_SCIPY_2023']] = 'array-apis'
conda_envs[dirnames['DIR_NUMPY']] = 'array-apis'
conda_envs[dirnames['DIR_NDINDEX']] = 'ndindex'
conda_envs[dirnames['DIR_VERSIONED_HDF5']] = 'versioned-hdf5'
conda_envs[dirnames['DIR_PYFLYBY']] = 'pyflyby3'
conda_envs[dirnames['DIR_BLOG']] = 'blog-nikola-pip312'
conda_envs[dirnames['DIR_SCIPY']] = 'scipy-dev'
conda_envs[dirnames['DIR_PAPYRI']] = '~/anaconda3/envs/papyri'
conda_envs[dirnames['DIR_CONDA_STORE_SERVER']] = 'conda-store-server-dev'
conda_envs[dirnames['DIR_PYTORCH']] = 'pytorch'

def main():
    # TODO: Support CDPATH

    if len(sys.argv) > 2:
        exit("Exactly one argument required")
    elif len(sys.argv) == 1:
        CDDIR = canon('~')
    elif sys.argv[1] == '-':
        CDDIR = canon(os.environ.get("OLDPWD", ''))
    else:
        CDDIR = canon(sys.argv[1])

    CONDA_DEFAULT_ENV = os.environ.get("CONDA_DEFAULT_ENV", "")

    envname = 'base'
    for dir in conda_envs:
        if CDDIR.startswith(dir):
            envname = conda_envs[dir]
            break

    if envname == CONDA_DEFAULT_ENV:
        return

    print('conda deactivate;')
    print('conda activate {envname};'.format(envname=envname))

if __name__ == '__main__':
    main()
