bin_PROGRAMS = dst2csv \
    dst2ascii

AM_FCFLAGS = -cpp -w -ffixed-line-length-none

f90getopt.o : f90getopt.f90 
dst2csv.o : dst2csv.f90 f90getopt.o
dst2ascii.o : dst2ascii.f90 f90getopt.o

dst2csv_SOURCES = f90getopt.f90 \
      dst2csv.f90

dst2ascii_SOURCES =  f90getopt.f90 \
      dst2ascii.f90

CLEANFILES = f90getopt.mod
