voacapl
=======

A port of VOACAP for Linux

This seeks to maintain a port of the VOACAP code base that may be compiled using GFortran and run on Linux.  

# Installation
The application requires the gfortran compiler which is present in the repositories most, if not all, modern Linux distributions. 

On Fedora, the compiler may be installed with the following command;
    $ sudo yum install gcc-gfortran
On Ubuntu, the following command may be used
    $ sudo apt-get install gfortran
##Build
Building the application follows the normal configure / make / install cycle followed by the command 'makeitshfbc' to create the data files required by voacap;
    $ ./configure
    $ make
    $ sudo make install
    $ makeitshfbc
