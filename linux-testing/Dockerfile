FROM continuumio/miniconda3

WORKDIR /root

RUN apt-get update \
    && apt-get install -y libc6-i386 libc6 linux-headers-amd64 git make git-core bash-completion \
    && apt-get -y clean

RUN conda config --add channels conda-forge
RUN conda config --set always_yes yes
RUN conda install emacs \
    && conda clean --all
RUN conda info

RUN git clone git://github.com/sympy/sympy.git

ENTRYPOINT ["bash"]
