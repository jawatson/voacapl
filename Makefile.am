#
#  Top Level Makefile for VOACAPL
#

SUBDIRS = src/hfmufesw\
	src/voa_lib\
	src/wp10dwin\
	src/voacapw\
	voacapl/itshfbc/coeffs\
	voacapl/itshfbc/bin/anttyp90 \
	voacapl/itshfbc/bin/dst
	
dist_doc_DATA = README.md\
		NEWS\
        COPYING\
		ChangeLog.md\
		AUTHORS\
		logo.png

nobase_dist_doc_DATA = voacapl/itshfbc/news/*

dist_man_MANS = man/voacapl.1

dist_bin_SCRIPTS = makeitshfbc

nobase_dist_data_DATA = \
	voacapl/itshfbc/antennas/default/ccir.* \
	voacapl/itshfbc/antennas/default/const17.voa\
	voacapl/itshfbc/antennas/default/isotrope\
	voacapl/itshfbc/antennas/default/swwhip.voa\
	voacapl/itshfbc/antennas/samples/sample.*\
	voacapl/itshfbc/areadata/default/default.*\
	voacapl/itshfbc/area_inv/default/default.*\
	voacapl/itshfbc/coeffs/*.asc\
	voacapl/itshfbc/database/cirafp.911\
	voacapl/itshfbc/database/colors.dat\
	voacapl/itshfbc/database/colors.win\
	voacapl/itshfbc/database/north_pole.txt\
	voacapl/itshfbc/database/version.w32\
	voacapl/itshfbc/database/version.win\
	voacapl/itshfbc/database/voacap.def\
	voacapl/itshfbc/geocity/*.geo\
	voacapl/itshfbc/geonatio/*.geo\
	voacapl/itshfbc/geostate/*.geo \
	voacapl/itshfbc/run/*.dat\
	voacapl/itshfbc/run/voaareax.da1\
	voacapl/itshfbc/run/voacapx.out\
	voacapl/itshfbc/run/voacapw.asc\
	voacapl/itshfbc/run/voacap.cir

install-exec-hook:
	sed "s|__PREFIX__|@prefix@|" $(DESTDIR)/$(bindir)/makeitshfbc > makeitshfbc.tmp
	chmod 755 makeitshfbc.tmp
	mv makeitshfbc.tmp $(DESTDIR)/$(bindir)/makeitshfbc

install-data-hook:
	$(DESTDIR)/$(bindir)/coeffbin $(DESTDIR)/$(datadir)/voacapl/itshfbc/coeffs &&\
	$(DESTDIR)/$(bindir)/coefbinw $(DESTDIR)/$(datadir)/voacapl/itshfbc/coeffs &&\
	$(DESTDIR)/$(bindir)/fof2bin $(DESTDIR)/$(datadir)/voacapl/itshfbc/coeffs &&\
	$(DESTDIR)/$(bindir)/fof2binw $(DESTDIR)/$(datadir)/voacapl/itshfbc/coeffs &&\
	echo "" &&\
	echo "" &&\
	echo "***************************************************" &&\
	echo "Run the command 'makeitshfbc' logged in with " &&\
	echo "your usual username to create the itshfbc directory" &&\
	echo "structure in your home directory." &&\
	echo "***************************************************" &&\
	echo "" &&\
	echo "" 


