      subroutine gh_exec(PROGRAM,nch,iwait)
c          PROGRAM = command line to execute (with parameters)
c          nch     = # characters in PROGRAM
c          iwait   = 0 = execute without wait
c                  = 1 = execute with wait until done
c*******************************************************************
      INCLUDE <WINDOWS.INS>
c********************************************************
      STDCALL MYCREATEPROCESS 'CreateProcessA' (VAL,STRING,VAL,VAL,
     +    VAL,VAL,VAL,VAL,REF,REF):LOGICAL*4

c          *** startup info structure ***
      character startup*68
      integer*4 cb,wShowWindow
      equivalence (cb,startup(1:4)),(wShowWindow,startup(49:50))

c          *** process info structure ***
      character process*16
      integer*4 hProcess
      equivalence (hProcess,process(1:4))
c********************************************************
      character PROGRAM*(*)
      integer*4 exit_code
c          *** initialize startup info structure ***
      call char_fill@(startup,char(0))
      cb=68
      wShowWindow=SW_SHOW
c**************************************************************
      if(.not.MyCreateProcess(0,PROGRAM(1:nch),0,0,0,0,0,0,
     +      startup,process)) then
         write(*,1) PROGRAM(1:nch)
1        format(' MyCreateProcess error in gh_exec during:',/,a)
         pause
      else                !  wait for WORLDWIN.EXE to finish
         if(iwait.eq.0) go to 999      !  do not wait
         exit_code=STILL_ACTIVE
         do while(exit_code.eq.STILL_ACTIVE) ! wait until [model]W.EXE finished
            call yieldit
            call GetExitCodeProcess(hProcess,exit_code)
         enddo
      end if
c**************************************************************
999   return
      END
c-----------------------------------------------------------------
