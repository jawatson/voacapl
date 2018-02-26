      subroutine file_nam(subdir,filenam,filesufx,filename)
c          concatenate to create the antenna type filename
      character filename*(*),subdir*(*),filenam*8,filesufx*3,t_subdir*12
      character(len=1), parameter :: PATH_SEPARATOR ='/'
c**********************************************************************
      t_subdir=subdir
      nch1=lcount(t_subdir ,12)
      nch2=lcount(filenam,8)
      nch3=lcount(filesufx,3)
      if(nch1.eq.0) nch1=1
      if(nch2.eq.0) nch2=1
      if(nch3.eq.0) nch3=1
      filename='..'//PATH_SEPARATOR//'antennas'//PATH_SEPARATOR//
     +         t_subdir(1:nch1)//PATH_SEPARATOR//filenam(1:nch2)//
     +         '.'//filesufx(1:nch3)
      return
      end
