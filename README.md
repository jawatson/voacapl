voacapl
=======

A port of VOACAP for Linux

This seeks to maintain a port of the VOACAP code base that may be compiled using GFortran and run on Linux.  The code is based on the material made publicly available by Greg Hang at [http://www.greg-hand.com/hfwin32.html](http://www.greg-hand.com/hfwin32.html).

This material was previously made available on my website at QSL.net [http://www.qsl.net/hz1jw/](http://www.qsl.net/hz1jw/).

# Installation
The application requires the gfortran compiler which is present in the repositories most, if not all, modern Linux distributions. 

On Fedora, the compiler may be installed with the following command;

    $ sudo yum install gcc-gfortran

On Ubuntu, the following command may be used

    $ sudo apt-get install gfortran

Initialise the build with the following commands;

    $ automake --add-missing
    $ autoreconf

## Build
Building the application follows the normal configure / make / install cycle followed by the command 'makeitshfbc' to create the data files required by voacap;

    $ ./configure
    $ make
    $ sudo make install
    $ makeitshfbc

## Running voacapl
The following command will run voacapl from the command line using the contents of the default input file, saving results to itshfbc/run/voacapx.out;

    $ voacapl ~/itshfbc 

Please refer to the [wiki](https://github.com/jawatson/voacapl/wiki) for further guidance on running voacapl.

### Additional Software
VOACAP is capable of exporting a significant amount of distance information, triggered when using an input file named 'voacapd.dat'.  The information is exported in the form of a Fortran records and is not readily accessible to other applications.  A couple of small utilities have been added in version 0.7, dst2csv and dst2ascii, to read this file and produce a text based report, either as a CSV for importing into external applictaions for further analysis, or as a (large) ASCII table.

The two application will be compiled and installed as part of a normal installation.

73's

Jim (M0DNS / HZ1JW)

