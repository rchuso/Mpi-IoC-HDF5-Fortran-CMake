FROM ubuntu:18.04

RUN apt-get update &&\
    apt-get -y install \
    git \
    software-properties-common \
    vim \
    cmake

RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test &&\
    apt-get update &&\
    apt-get install -y \
    gfortran-7 \
    libmpich-dev \
    libhdf5-dev

RUN git clone --depth=1 https://github.com/rchuso/Mpi-IoC-HDF5-Fortran-CMake.git
WORKDIR /Mpi-IoC-HDF5-Fortran-CMake
CMD ["/bin/bash"]
