      subroutine rdCIRAF911(cxlat,cxlon,n)  !  read CIRAF test point file
      use voacapl_defs
      use crun_directory
      dimension cxlat(*),cxlon(*)
c      common /crun_directory/ run_directory
c         character run_directory*50
      character ns*1,ew*1
      data lu/49/
      n=0
      nch_run=lcount(run_directory,50)
      open(lu,file=trim(root_directory)//PATH_SEPARATOR//'database'//PATH_SEPARATOR//'cirafp.911',status='old',err=900)
      rewind(lu)
10    read(lu,11,end=100) lat,ns,lon,ew
11    format(4x,i2,a1,i3,a1)
      n=n+1
      if(ns.eq.'S') lat=-lat
      if(ew.eq.'W') lon=-lon
      cxlat(n)=lat
      cxlon(n)=lon
      go to 10
100   close(lu)
900   return
      end
c------------------------------------------------
      subroutine rdCIRAFXXX(cxlat,cxlon,n)  !  read CIRAF test point file
      use voacapl_defs
      use crun_directory
      dimension cxlat(*),cxlon(*)
c      common /crun_directory/ run_directory
c         character run_directory*50
      character ns*1,ew*1
      data lu/49/
      n=0

      nch_run=lcount(run_directory,50)
      open(lu,file=trim(run_directory)//PATH_SEPARATOR//'cirafp.XXX',status='old',err=900)
      rewind(lu)
10    read(lu,11,end=100) lat,ns,lon,ew
11    format(4x,i2,a1,i3,a1)
      n=n+1
      if(ns.eq.'S') lat=-lat
      if(ew.eq.'W') lon=-lon
      cxlat(n)=lat
      cxlon(n)=lon
      go to 10
100   close(lu)
900   return
      end
c-----------------------------------------------
