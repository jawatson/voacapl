      subroutine getdirs(basedir,list,nmax,num)
c*******************************************************************
c         get list of antenna file directories
c         under the directory "basedir"
c*******************************************************************
      character basedir*(*),based*20,dirname*12,list(*)*14
      based=basedir
      nch=lcount(based,20)
      ier=isystem('dir '//based(1:nch)//' /one /A:D /b /-p>dirs.lst',
     +      30+nch)
      open(31,file='dirs.lst',status='old',err=900)
      rewind(31)
      num=0
10    read(31,'(a)',end=100) dirname
      num=num+1
      list(num)=' '//dirname//' '
      if(num.lt.nmax) go to 10
100   close(31)
      ier=isystem('del dirs.lst',12)
      return
900   stop 'In GETDIRS Could not OPEN file=dirs.lst'
      end
c---------------------------------------------------------------
