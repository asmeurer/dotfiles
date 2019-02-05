FROM continuumio/miniconda3

WORKDIR /root

RUN apt-get update \
    && apt-get install -y libc6-i386 libc6 linux-headers-amd64 git make git-core bash-completion freeglut3-dev\
    && apt-get -y clean

RUN conda update conda
RUN conda config --add channels conda-forge
RUN conda config --set always_yes yes
RUN conda install emacs mpmath ipython conda-build numpy scipy matplotlib \
    && conda create -n python2 python=2 mpmath ipython \
    && conda clean --all
RUN conda info

RUN git clone --progress --verbose git://github.com/sympy/sympy.git

WORKDIR /root/sympy

RUN git remote add github https://github.com/asmeurer/sympy
RUN git fetch github

WORKDIR /root

ENTRYPOINT ["bash"]
