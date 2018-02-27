c# DOScolr.for
      subroutine DOScolr    !  read DOS color table
      common /crun_directory/ run_directory
         character run_directory*50
      common /DOS_col/ ncolors,colors(20),index_DOS(20),DOScol(20)
      character colors*6,DOScol*6
      character skip*1
C------------------------------------------------------------------
      nc=0
      nch_run=lcount(run_directory,50)
      open(17,file=run_directory(1:nch_run-3)//'database/colors.dat',
     +     status='old',err=900)
      rewind(17)
      read(17,'(a)') skip    !  skip header card
10    read(17,11,end=100) colors(nc+1),index_DOS(nc+1),DOScol(nc+1)
11    format(a6,i5,2x,a6)
      nc=nc+1
      if(nc.lt.20) go to 10
100   close(17)
900   ncolors=nc
      return
      end
