c------------------------------------------------------------
      subroutine rdCIRAF    !  read the ../database/cirafp.911 file
      common /Cciraf/ nciraf_tp,ciraf(911),ciraf_lat(911),ciraf_lon(911)
         character ciraf*4
c          this contains rectangle containing CIRAF zones
      common /Cciraf_max/ ciraf_lat_max(4,85),ciraf_lon_max(4,85)
      common /crun_directory/ run_directory
         character run_directory*50
      character ns*1,ew*1

      nch_run=lenchar(run_directory)

      nciraf_tp=911
      open(31,file=run_directory(1:nch_run-3)//
     +        'database/cirafp.911',status='old',err=900)
      rewind(31)
      do 10 itp=1,nciraf_tp
      read(31,1) ciraf(itp),latd,ns,lond,ew
1     format(a4,i2,a1,i3,a1)
      if(ns.eq.'S') latd=-latd
      if(ew.eq.'W') lond=-lond
      ciraf_lat(itp)=latd
10    ciraf_lon(itp)=lond
      close(31)

      open(31,file=run_directory(1:nch_run-3)//
     +        'database/cirafp.max',status='old',err=910)
      rewind(31)
      do 30 izn=1,85
      do 20 j=1,2
      read(31,11) latd,ns,lond,ew
11    format(4x,i2,a1,i3,a1)
      if(ns.eq.'S') latd=-latd
      if(ew.eq.'W') lond=-lond
      ciraf_lat_max(j,izn)=latd
20    ciraf_lon_max(j,izn)=lond
      ciraf_lat_max(3,izn)=ciraf_lat_max(1,izn)
      ciraf_lon_max(3,izn)=ciraf_lon_max(2,izn)
      ciraf_lat_max(4,izn)=ciraf_lat_max(2,izn)
      ciraf_lon_max(4,izn)=ciraf_lon_max(1,izn)
30    continue
      close(31)
      return
c***********************************************************
900   write(*,901) '../database/cirafp.911'
901   format(' In rdciraf, could not OPEN file=',a)
      pause 'Could not OPEN file ../database/cirafp.911'
      go to 999
910   write(*,901) '../database/cirafp.max'
      pause 'Could not OPEN file ../database/cirafp.max'
999   return
      end
c------------------------------------------------------------
