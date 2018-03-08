      subroutine del_abt     !  delete the voaarea.abt & voacap.abt files
      use crun_directory
c**********************************************************
c jw      common /crun_directory/ run_directory
c jw         character run_directory*50
      nch_run=lcount(run_directory,50)
c          make sure the ABORT file is not there
      call erase@(run_directory(1:nch_run)//'\voaarea.abt',error_code)
      call erase@(run_directory(1:nch_run)//'\voacap.abt',error_code)
      return
      END
* -------------------------------------------------------------------- *
