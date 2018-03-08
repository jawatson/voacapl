* --------------------------------------------------------------------- *
      FUNCTION iant_idx(dir,file)     !  open file and get antenna index
* --------------------------------------------------------------------- *
      use voacapl_defs
      use crun_directory
c jw      common /crun_directory/ run_directory
c jw         character run_directory*50
      CHARACTER tempstring*1,dir*8,file*12,antfil*21
      iant_idx=-1
      call antfile(dir,file,antfil)
      nch_run=lcount(run_directory,50)
      open(31,file=run_directory(1:nch_run-3)//'antennas'//PATH_SEPARATOR//antfil,
     +     status='old',err=100)
      rewind(31)
      read(31,'(a)') tempstring
      read(31,'(a)') tempstring
      read(31,'(a)') tempstring
      read(31,*) iant_idx          !  antenna index
      close(31)
100   return
      end
