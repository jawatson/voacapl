AC_INIT([voacapl], [0.7.4], [jwatson@neomailbox.ch])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_F77([gfortran])
AC_PROG_LN_S
AC_PROG_FC([gfortran])
AC_PROG_RANLIB
AC_CONFIG_FILES([
 	Makefile
 	src/modules/Makefile
 	src/hfmufesw/Makefile
 	src/voa_lib/Makefile
 	src/wp10dwin/Makefile
 	src/voacapw/Makefile
	src/rec533/Makefile
	voacapl/itshfbc/coeffs/Makefile
	voacapl/itshfbc/bin/anttyp90/Makefile
	voacapl/itshfbc/bin/anttyp99/Makefile
	voacapl/itshfbc/bin/dst/Makefile
])
AC_OUTPUT

