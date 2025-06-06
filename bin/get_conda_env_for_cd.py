#!/usr/bin/env python

import sys
import os

def canon(p):
    return os.path.realpath(os.path.expanduser(p)) + os.path.sep

HOME = canon('~')

dirnames = dict(
    DIR_ARRAY_API=f"{HOME}Documents/array-api",
    DIR_ARRAY_API_TESTS=f"{HOME}Documents/array-api-tests",
    DIR_ARRAY_API_COMPAT=f"{HOME}Documents/array-api-compat",
    DIR_ARRAY_API_STRICT=f"{HOME}Documents/array-api-strict",
    DIR_SCIPY_2023=f"{HOME}Documents/scipy-2023-presentation",
    DIR_NUMPY=f"{HOME}Documents/numpy",
    DIR_NDINDEX=f"{HOME}Documents/ndindex",
    DIR_VERSIONED_HDF5=f"{HOME}Documents/versioned-hdf5",
    DIR_PYFLYBY=f"{HOME}Documents/pyflyby",
    DIR_BLOG=f"{HOME}Documents/blog",
    DIR_SCIPY=f"{HOME}Documents/scipy",
    DIR_PAPYRI=f"{HOME}Documents/papyri",
    DIR_CONDA_STORE_SERVER=f"{HOME}Documents/conda-store/conda-store-server",
    DIR_PYTORCH=f"{HOME}Documents/pytorch",
    DIR_RECIPES_CLEANUP=f"{HOME}Documents/recipes-cleanup",
    DIR_MATPLOTLIB=f"{HOME}Documents/matplotlib",
)

conda_envs = {}

conda_envs[dirnames['DIR_ARRAY_API']] = 'array-apis'
conda_envs[dirnames['DIR_ARRAY_API_TESTS']] = 'array-apis'
conda_envs[dirnames['DIR_ARRAY_API_COMPAT']] = 'array-apis'
conda_envs[dirnames['DIR_ARRAY_API_STRICT']] = 'array-apis'
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
conda_envs[dirnames['DIR_RECIPES_CLEANUP']] = 'recipes-cleanup'
conda_envs[dirnames['DIR_MATPLOTLIB']] = 'mpl-dev'

def main():
    # TODO: Support CDPATH

    PWD = os.environ.get("PWD")
    if not PWD:
        sys.exit("Could not get $PWD")
    PWD = canon(PWD)

    CDDIR = canon(PWD)

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
