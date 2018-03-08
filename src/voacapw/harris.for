      subroutine harris(jant,fileant,mode,idx,antfile,fs,fe,
     +                  beam_main,offazim)
c*******************************************************************
c     11/98 Greg Hand modifications for Harris
c
c     Given an external antenna type xx (90-99),
c     1. create a data file ANTTYPxx.DAT
c     2. execute the program ..\BIN_WIN\ANTTYPxx.EXE rundirectory mode
c        where:
c              rundirectory = C:\ITSHFBC\RUN (default installation)
c              mode = (blank) = point-to-point
c                   = a = area coverage
c*******************************************************************
      use voacapl_defs
      use crun_directory
      CHARACTER fileant*10,antfile*21,mode*1
c jw      common /crun_directory/ run_directory
c jw         character run_directory*50
      dimension areagain(91)
      character program*300,alf*1,exe*12,dat*12
      logical*1 iexe_exist
      nch_run=lcount(run_directory,50)

#ifdef _WIN
      write(exe,'(6hANTTYP,i2.2,4h.EXE)') jant
      write(dat,'(6hANTTYP,i2.2,4h.DAT)') jant
#else
      write(exe,'(6hanttyp,i2.2)') jant
      write(dat,'(6hanttyp,i2.2,4h.dat)') jant
#endif

C
C**********Harris modification M Packer for Area Coverage 11 May 1998 *******
C
c        This handles the Harris Type 99 antenna files.  Call the external
c        routines (written in C) to prepare the
c        GAINxx.DAT for area coverage.  Then read in the file.
c        We're gonna use numbers 250-299 if necessary.

c           Follow the same routine necessary for AREA type 13 antenna files.

c           The difficulty here is that antcalc is writing the GAINxx.DAT
c           file on lu 22 and we want to call a routine to generate the
c           GAINxx.DAT file.

c           Prepare ANTTYPxx.DAT to communicate with ANTTYPxx.EXE:
      open( 99, file=run_directory(1:nch_run)//PATH_SEPARATOR//dat,err=900)
      write( 99, 289 ) idx, antfile, fs,fe,beam_main,offazim
289   format( i2, 28x,   '=antenna number (idx)' /
     2       a21,  9x,  '=antenna file filename' /
     3      f7.3, 23x, '=antenna starting frequency (MHz)' /
     4      f7.3, 23x, '=antenna ending frequency (MHz)' /
     5      f7.2, 23x, '=antenna main beam (deg)' /
     6      f7.2, 23x, '=antenna off azimuth angle (deg)' )
      close( 99 )

c**********************************************************************
c          Make sure ANTTYPxx.EXE exists before executing it.
c**********************************************************************
c jw      iexe_exist=it_exist(run_directory(1:nch_run-3)//'bin_win\'//exe)
#ifdef _WIN
      inquire(file=run_directory(1:nch_run-3)//'bin_win'//PATH_SEPARATOR//exe,exist=iexe_exist)
c jw      if(iexe_exist.ne.1) then    !  antenna program does not exist
      if(.NOT.iexe_exist) then    !  antenna program does not exist
         write(*,1) jant,run_directory(1:nch_run-3)//'bin_win\'//exe
1        format('You are attempting to use an antenna of type=',i4,'.',/
     +   'That requires an external antenna calculation program.',/
     +   'The required program=',a,'  does not exist.'/
     +   'Please correct the problem and try again.'//)
         stop 'Required external antenna program does not exist.'
      end if
c**********************************************************************
C        Call ANTTYPxx.EXE to create GAINxx.DAT file:
C
      PROGRAM=run_directory(1:nch_run-3)//'bin_win'//PATH_SEPARATOR//exe//' '//
     +             run_directory(1:nch_run)//' '//mode
#else
      PROGRAM=exe//' '//run_directory(1:nch_run)//' '//mode
#endif
      nch=lcount(PROGRAM,300)
c jw      call gh_exec(PROGRAM,nch,1)   !  execute and wait for ANTTYPxx.EXE
      call system(PROGRAM)
      if(mode.eq.' ') return     !  point-to-point is done

C           Open GAINxx.DAT file
      open(22,file=run_directory(1:nch_run)//PATH_SEPARATOR//fileant,
     1            status='old', err=910)
      rewind(22)
      read(22,'(a)') alf
      read(22,'(a)') alf
      read(22, 292 ) freq, eff
292   format( 10x, f7.3, 10x, f10.3 )


c           Read the rest of the file which contains antenna gain info
      do iazim=0,359
c jw         call yieldit
         read( 22, 293 ) ( areagain(ielev), ielev=1,91 )
293      format( 9x, 10f7.3 )
         call antsave( idx, iazim, freq, eff, areagain )
      end do

c           We are done.  Close up, take your ball, and go home.
      close( 22 )
      return
c*****End Harris mod      *************************************************
900   write(*,901) run_directory(1:nch_run)//dat
901   format('Could not create file:',a)
      go to 920
910   write(*,911) run_directory(1:nch_run)//PATH_SEPARATOR//fileant
911   format('Could not OPEN file:',a)
920   write(*,921)
921   format('Failure in subroutine harris')
      stop 'Fatal error in subroutine harris'
      end
c-------------------------------------------------------
