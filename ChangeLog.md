## Changelog
### v0.7.3 (In Progress)
* Added support for the --run-dir and --root-dir command line arguments to permit specifying the location of the and root directories respectively.  This was requested by Jari to keep user's files separate when running in a server environment.
* Added the --absorption-mode command line parameter to permit the absorption mode to be specified from the command line (W = normal code, I = use old IONCAP absorption model, A = use Alex's changes with normal absorption or a = use Alex's changes with IONCAP absorption).
* Added invcon.for to the build.  This file was accidentally omitted from previous builds.
* Adds anttype99 to the build.  This is derived from the anttype90 code published with voacap and supports Harris type 99 antenna files.  No type 99 files are provided with this distribution.

### v0.7.2 04Mar18
* Fixed a bug that was preventing antenna type 90 file from working correctly.
* Fixed a bug in the dst2xxx applications caused by using a variable in the 
stop message.  This has been replaced with a write string followed by a stop
with no parameters.
* Added support for up to 25 area files to be processed in a single run. The
limit is defined by the variable MAX\_AREA\_MONTHS in the definitions module.
* Removed a warning when compiling the modules by suppressing the linking.
* The version number is now read into the voacapls\_def module from a pre-processor macro, VERSION which is the version number saved in the configure/ac file.
* Adds a check that the specified path to the itshfbc directory exists.
* Modified a couple of the Makefile.ac to remove wildcard characters which were causing problems with make distcheck.  Added a check to remove the compiled coeff files.  Running 'make distcheck' now passes. 


### v0.7.1 26Feb18
* Moved the definitions module to a separate directory to make sure it gets compiled before everything else.  This was breaking first time compiles on a new install.
### v0.7.0 26Feb18
* A number of bugs have been found in the recent releases that were easier to fix by simply returning to the latest available code released by Greg and making the smallest amount of changes possible to get the code running on Linux. This release should be regarded as a fresh start and it's recommended that all users update to this version.

---

### v0.6.7 09May15
* Fixes a bug that I introduced when changing the iharris variable to a logical type.  The output files are now closer to the Windows version.

### v0.6.6 07May15
* Multiple changes using the latest available code from Greg's website.  This should align with the Windows version v15.0605.

### v0.6.5 30Sep14 
* Modified files antcalc.f and relbil.f using source files kindly supplied by Greg Hand to bring this version into line with VOACAP 14.0905

### v0.6.4 19Jan14 
* Fixed a bug in which "Number of months" was printed when performing area calcs, irrespective of the -s flag.  Fixed a typo which was causing the lat/lons to be printed incorrectly when performing area plots.

### v0.5.5 27Nov09
* Incorporated a patch from Thomas Beirlein (DL1JBE) to fix a bug in the Makefile.am.  Added support for up to 25 area plots to be calculated.  Ths number of plots supported is now defined at compile time by the parameter MAX_AREA_MONTHS in the file voa_defs.hdr. 

### v0.5.4 05Nov09
* Modified the Makefile to remove a duplicate entry to the voacapg.dat file that was causing problems with the newer autotools distributed with Ubuntu 9.10.

### v0.5.2 04Sep09
* Tweaked the distribution a little to correct a few errors in the install.

### v0.5
- The itshfbc directory structure is now packaged with the main application. 
- Changed the variable 'iharris' type to logical, making it suitable for use with inquire.  This permits the 'it_exist.f' to be removed. 
- ftnchek has been used to help cleanup some of the code in voa_lib 
- dazel1: Variables set but never used: TLONR set at line 59 file voa_lib/dazel1.f.    dazel1: Labels defined but not used: <200> defined at line 58 file voa_lib/dazel1.f. voacapw.f: 
- Removed the ierase variable used to hold debug windows open.

### v0.4.7 14Aug09
- The correct versions of fobby.f and fdist.f are now being used. The earlier versions were causing some minor discrepancies between the Windows and Linux versions.

### v0.4.6 03Apr09
- Updated mpath.f with latest version published by Greg Hand.

### v0.4.5 14Mar09
- Changed the configure.ac to only look for gfortran. Some users have g77 installed which is unable to compile F90.

### v0.4.4 15Aug08
- Added a few checks to make sure that the input files exist when performing area calculations.

### v0.4.3
* *vg files are now correctly deleted (previous versions didn't delete files from 10 onwards.

### v0.4.1 25Apr08
- Tested on Ubuntu 8.04 (gcc 4.2.3) and Fedora 8 (4.1).
- Removed some '1x1A' characters that had crept into the tail end of some files. This wasn't a problem on Fedora - I only noticed when trying to compile on my Ubuntu machine (GFortran 4.2).
- Updated to include changes made by Greg, described in news_win/08_04_10.txt. Area calculations now produce results in the range 0/360 or -180/180, according to the users origianl input. (Note: the python plotting scripts assume a range of -180/180)
- Running ftnchek identified a few files that don't get called in the Linux version. These have been removed these from the distribution; (voacapw) getkmf.f, gh_exec.f, lcase.f, ucase.f, (wp10dwin) getdirs.f, file_nam.f, curnorm.f, iant_idx.f, getantyp.f, getfiles.f, antinit.f, dirgain.f, (voa_lib) yieldit.f
- The file cisi.f was listed and compiled twice (in voacapw & wp10dwin). The copy in the voacapw directory has been removed.
- Replaced all 'stop' with a call to 'exit()' which allows a return value to be returned to a calling program which may be of use to a GUI. Return codes are defined in modules/errorcodes.f
- The external abort mechanism (using the .abt files) has been removed, along with the 'cancel_batch' subroutines

### v0.4 18Apr08
* This version came about following the discovery of a few bugs with a few .dat files. While trying to locate the bugs it quickly became apparent that it would be quicker to create a fresh port from the latest Windows version than to actually locate the bugs. This version is about as close to the original source as is possible and will be the breakpoint for future regressions. Work to port the source to F90/95 standards will continue but at a slower rate to allow for more testing between releases. In addition to the standard Windows functionality, this version supports the -s (silent) and -v (version) flags. It also allows up to 12 files to be specified in an area calculation.

### v0.3.3 14Apr08
- The build system now uses GNU Automake.  This is the first step towards adding a few processor dependant compiler optimisation flags for 32bit and 64 bit machines.  I've noticed that I get slightly different results (+/- 0.1) on my x86_64 machine than my 32 bit machine, which gave exactly the same results as the Windows versions.
- The recent code changes made by Greg Hand have now been replicated over to align this version with Windows version '080410'.  The changes include a modification to allow users to specify the location of magnetic North.  Refer to the Windows notes for details. Many thanks to Greg for sharing the changes via his website. The files voacapw.f, geom.f, harris.f and gridxy.f were modified in accordance with the source published on Greg's site.

### v0.3.2 11Apr08
- Nothing major, just tidied up a few of the messages displayed on the screen at runtime.

### v0.3.1 08Dec07
- Modified a number of files to permit path lengths to the itshfbc directory of up to 255 characters. Spaces in path names didn't seem to be a problem during tests.

### v0.3 05Dec07
- All warnings have been resolved (when compiled with Fedora GFortran 4.1.2-27)
- Added a (basic) man page to the distribution.
- Added support for a '-v' option to print the version number.
- Modified gphbod.f to remove a warning about jumping out of an 'if' control block. (This warning was the result of earlier refactoring around 0.2.6 that removed the arithmetic if statements).
- Modified order and size of some of common variables in fice_ssn.hdr, ficehdr.hdr and ficearea.hdr to align against 8 byte boundaries.
- Removed unused 'fileout' variable from hfmufs2.f (and call from voacapw.f)
- Removed unused 'freq' parameter from 'antsave' and corresponding calls from harris.f and antcalc.f.
- Removed unused 'filein' parameter from batch.f and batch_s.f and calls from voacapw.f.
- Refactored fdist.f to remove arithmetic if statements. (This file needs revisiting, the arithmetic ifs have simply been replaced with some rather clumsy code).
- Initialised 'agauss' to zero in agauss.f
- Modified order of Common block 'WIN_col' in wincolr.f to align on 8 byte boundaries.

### v0.2.9 26Nov07
- lngpat.f, luffy.f, inmuf.f, cang.f, penang.f, xlin.f, matinv.f, find.f: re-factored to remove arithmetic if statements.
initialised the following variables to remove warnings at compile time;
- 'ofaz' in gain.f
- 'CZ' in genfam.f
- 'sbv', 'sav', 'sbu' & 'sau' in dirgain.
- Removed unused variable 'npsl' from distxy.f (and call from hfmufs2.f)

### v0.2.8 20Sep07
- Started to re-factor findf.f & curmuf.f
- cngtime.f, f2dis.f, f2var.f, frqcom.f, esmod.f, syssy.f, alosfv.f, anois1.f, babs.f, ef1var.f, genion.f: re-factored to remove arithmetic if statements.
- Unreachable code in esreg.f deleted to remove warnings when compiling with -Wall.

### v0.2.7 Sept07
- Modified hfarea.f to correct a formatting error introduced in a recent version.
- inmuf.f, penang.f:started to remove arithmetic mean statements.
- seltxr.f, gethp.f, geom.f, fnorml.f, lecden.f, setout.f, ionplt.f, setluf.f, settxr.f, gmloss.f, versy.f, setgph.f, ionset.f, gettop.f: refactored to remove arithmetic mean statements.
- Corrected a typo in tabbod.f in the assignment of IGMT.

### v0.2.6 Sept07
- Started to refactor grpbod.f
- noisy.f, decred.f, tabs.f, regmod.f, geotim.f, magvar.f, setlng.f, magfin.f, cisi.f, dazel.f, sigdis.f refactored to remove arithmetic if statements
- hfciraf.f, hfarea.f - Added and error codes to the 'unlink' statements and deleted some variables that can't be called in the GFortran implementation.

### v0.2.5 Sep07
- Modified the voacap.f, areamap.f, hfmufs.f, hfmufs2.f in line with Greg's changes to the main source.
- outpar.f, sang.f, outmuf.f, timvar.f, outlay.f, outall.f, outbod.f - Refactored to remove arithmetic if statements.
- voacapl.f - fixed a typo introduced in an earlier version that meant start_time was not initialised correctly. Initialised the istat variables correctly in the Linux specific unlink commands.

### v0.2.4 Sep07
No major changes just revised a few lines;
- Fixed a couple more ('/') file seperators in wincolr.f error messages.
- Fixed a couple of case/file seperators in set_run.f
- Deleted unused label (200) in dazel1.f
- Modified delarations (kind=n) in itexist.f, curtain.f, dirgain.f
- Commented out the 'pause' in getfiles.f
- Deleted unused variable 'gains' from antinit.f
- Factored out the aritmetic if statements in dazel0.f, iongain.f and xlin.f mufsegan.f:495 deleted unused label '278'
- mufsegan.f:497 deleted unused label '279'
- mufsegan.f:620 deleted unused label '620'

### v0.2.3 Aug07
- Added the following lines to the hfmufs.f file; meth=METHOD if(mspec.eq.121) meth=30 The 'meth' variable is then used in the calls to write. This puts the correct method on the screen when Method 30 is called.

### v0.2.2 Aug07
- Revised code to permit reading of up to 12 groups/months in an area calcuation;
- areamap.f:26 card*100 !card length increased to 100 to accomodate additional input lines
- areamap.f:146 do 20 nmonths=1,12 ! find out how many months
- areamap.f:158 do 500 ii=1,12 ! create a file for each plot
- areamap.f: Increased size of 'sufix' to 5 (sufix*5)
- ficearea; !Increased size of matrix to 12 (months)
- REAL*4 montha(12)
- INTEGER*4 ssna(12),ihour(12)
- real*4 qindexa(12)
- REAL*4 Freq(12)
- voacapw.f:Revised file numbering scheme to use an integer variable (fileNumCtr) and 'write' to create the file names (around line 536)

### v0.2.1 Aug07
* Made a few changes in the hfarea.f:113 file to tidy up the progress display.  The advance='no' option (available in F90) has been used instead of the sou@ function which is not supported in GFortran.

### v0.2 Aug07
* Changed case of the 'Version.w32' file-name to 'version.w32' (lowercase 'V'). Changed case of the gain01/02 files created in the itshfbc/run directory. Changed the case of all source code file names.

### v0.1.3 July07
* Fixed a number of bugs in the voa area routines. Added the 'make install' option to the Makefile.

### v0.1.2a Jul07
* Corrected an error in VOACAPW.f:428 that prevented the application compiling with GFortran >4.2. Added compilation option '-ffixed-line-length-none' to make the code a little easier to read.

### v0.1.1a Jun07
Refactored code in the following files to remove obsolete arithmetic if statements;
- CHARDEG.f:50
- DAZEL1.f:110
- MUFESGAN.f:492
- The 'SILENT' argument has been revised to '-s' e.g. 'voacapl -s ~/itshfbc inputFile.dat outFile.out'.
- Corrected a file separator error in ANTCALC.f:111 & DECRED.f:287 that was causing the GAINxx.DAT files to be written to the itshfbc directory. These files are now being (correctly) written to the itshfbc/run directory.

### v0.1a Jun07
* Initial public release. Tested on Fedora 7. 
