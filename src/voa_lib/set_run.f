      subroutine set_run        !  set current directory to ..\RUN
      common /crun_directory/ run_directory
      character run_directory*50

      integer error_code
      character tmp*50,subs(9)*10
      data subs/'/antennas/','/areadata/','/coeffs/'  ,
     +'/database/','/geocity/' ,'/geonatio/','/geostate/',
     +'/news_win/','/userdb/'  /

      nch=lcount(run_directory,50)
      if(run_directory(nch-3:nch).eq.'/RUN') go to 900 !  ok
      if(run_directory(nch-3:nch).eq.'/run') go to 900 !  ok
c     force this to be the RUN sub-directory
      tmp=run_directory

      do 10 i=1,12
         nch=lcount(subs(i),10)
         idx=index(tmp,subs(i)(1:nch))
         if(idx.le.0) go to 10
         run_directory=tmp(1:idx)//'run'
         go to 900
 10   continue
 900   call chdir(run_directory,error_code)    !  reset to the RUN directory 
      return
      end
c--------------------------------------------------------------------
      subroutine get_run       !  get current directory
      common /crun_directory/ run_directory
      character run_directory*50
      character tmp*50
      call getcwd(run_directory)      !  get the current directory
      nch=lenchar(run_directory)
c      if(run_directory(nch-7:nch).eq.'/bin_win')
c     +          run_directory(nch-6:)='run'
      if(run_directory(1:2).ne.'//') return
      if(run_directory(4:4).eq.'/') then
         nch_run=lcount(run_directory,50)
         tmp=run_directory(3:3)//':'//run_directory(4:nch_run)
         run_directory=tmp
      end if
      nch=lenchar(run_directory)
c      call ucase(run_directory,nch)
      return
      end
c--------------------------------------------------------------------
c     subroutine get_run       !  get current directory
c     common /crun_directory/ run_directory
c        character run_directory*50
c     character curdir@*50,tmp*50
c     run_directory=curdir@()      !  get the current directory
c     if(run_directory(1:2).ne.'\\') return
c     if(run_directory(4:4).eq.'\') then
c        nch_run=lcount(run_directory,50)
c        tmp=run_directory(3:3)//':'//run_directory(4:nch_run)
c        run_directory=tmp
c     end if
c     return
c     end
c--------------------------------------------------------------------
