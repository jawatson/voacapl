      subroutine rdCIRAF911(cxlat,cxlon,n)  !  read CIRAF test point file
      dimension cxlat(*),cxlon(*)
      common /crun_directory/ run_directory
         character run_directory*50
      character ns*1,ew*1
      data lu/49/
      n=0

      nch_run=lcount(run_directory,50)
      open(lu,file=run_directory(1:nch_run-3)//'database\cirafp.911',
     +        status='old',err=900)
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
      dimension cxlat(*),cxlon(*)
      common /crun_directory/ run_directory
         character run_directory*50
      character ns*1,ew*1
      data lu/49/
      n=0

      nch_run=lcount(run_directory,50)
      open(lu,file=run_directory(1:nch_run)//'\cirafp.XXX',
     +        status='old',err=900)
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