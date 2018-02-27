      subroutine getfiles(directory,list,nmax,num,*)
c*******************************************************************
c         get list of antenna file names
c         from the directory ..\ANTENNAS\directory
c*******************************************************************
      use voacapl_defs
      character directory*8,filename*12,description*63,list(*)*77
      nchdir=lcount(directory,8)
c jw TODO Look at the folloing to see if it's required...
      ier=isystem('dir ..\antennas\'//directory(1:nchdir)//
     +            '\*.* /one /b /-p>antfiles.lst',16+nchdir+29)
      open(31,file='antfiles.lst',status='old',err=900)
      rewind(31)
      num=0
10    read(31,'(a)',end=100) filename
      if(filename(1:8).eq.'Path not') go to 920     !  directory does not exist
      open(32,file='..'//PATH_SEPARATOR//'antennas'//PATH_SEPARATOR//directory(1:nchdir)//PATH_SEPARATOR//filename,
     +       status='old',err=910)
      rewind(32)
      read(32,'(a)') description
      close(32)
      num=num+1
      list(num)=' '//filename//' '//description
      if(num.lt.nmax) go to 10
100   close(31)
      ier=isystem('del antfiles.lst',16)
      return
900   stop 'In GETFILES Could not OPEN file=antfiles.lst'
910   write(*,911) '..'//PATH_SEPARATOR//'antennas'//PATH_SEPARATOR//directory(1:nchdir)//PATH_SEPARATOR//filename
911   format(' Could not OPEN file=',a)
      pause 'OPEN error in getfiles'
      return
920   close(31)           !  directory does not exist
      return 1
      end
c---------------------------------------------------------------
