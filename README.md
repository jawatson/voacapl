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

##Build
Building the application follows the normal configure / make / install cycle followed by the command 'makeitshfbc' to create the data files required by voacap;

    $ ./configure
    $ make
    $ sudo make install
    $ makeitshfbc

##Running voacapl
The following command will run voacapl from the command line using the contents of the default input file, saving results to itshfbc/run/voacapx.out;

    $ voacapl ~/itshfbc 

###Absorption Model
Most users will be happy with the dafault absorption model.  Some user's may 
however wish to experiment with alternative models which may be selected
by modifying the contents of the file itshfbc/database/version.w32

*  Version 14.0905W = normal code
*  Version 14.0905I = use old IONCAP absorption model
*  Version 14.0905A = use Alex's changes with normal absorption
*  Version 14.0905a = use Alex's changes with IONCAP absorption


73's

Jim (M0DNS / HZ1JW)

