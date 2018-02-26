      function it_exist(file)
c***************************************************************
c          if FILE does     exist, it_exist=1
c          if FILE does not exist, it_exist=0
c***************************************************************
      character file*(*)
ccc      integer*2 idate,itime
      logical doesit*4,fexists@*4
      integer*4 error_code
      it_exist=1
ccc      call get_file_date_time_stamp@(file,idate,itime)
ccc      if(idate.eq.-1) it_exist=0      !  file does not exist
      doesit=fexists@(file,error_code)
      if(.NOT.doesit) it_exist=0      !  file does not exist
      return
      end
c-----------------------------------------------------------
