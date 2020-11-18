bin_PROGRAMS = anttyp99

AM_FFLAGS = -w -ffixed-line-length-none

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

cant99.o : cant99.f95
ant99.o : ant99.f95 cant99.o
anttyp99.o : anttyp99.f95 cant99.o

anttyp99_SOURCES = cant99.f95\
	anttyp99.f95\
	ant99.f95

clean-local:
	-rm *.mod

