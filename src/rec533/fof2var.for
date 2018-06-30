ccc      program fof2var     !  read fof2 variability tables
ccc
ccc      data lu/61/
ccc      call read_fof2_var(lu)    !  read the foF2 variability tables
ccc10    write(*,'(''Input local_time, latitude, month, ssn = '',$)')
ccc      read(*,*,end=100) xlt,xlat,month,ssn
ccc      call get_fof2_var(xlt,xlat,month,ssn,dl_v,du_v)
ccc      write(*,'(''dl='',f8.3,''  du='',f8.3)') dl_v,du_v
ccc      go to 10
ccc100   continue
ccc      end
c------------------------------------------------------------
      subroutine read_fof2_var(lu)    !  read the foF2 variability tables
      use crun_directory
      use voacapl_defs
      character alf*80
      dimension var(24,19,18)
c*************************************************************************
      common /C_fof2_var/ ionce_fo,dl_fof2(24,19,3,3),du_fof2(24,19,3,3)
c234567890123456789012345678901234567890123456789012345678901234567890
c         dl = lower decile foF2 variability
c         du = upper decile foF2 variability
c         24 = 24 local time
c         19 = latitude   lat=(i-1)*5 degrees
c          3 = SSN  1=<50  2=50to100  3=>100
c          3 = season  1=winter(11,12,1,2)  2=equinox  3=summer(5,6,7,8)
c*************************************************************************
      equivalence (var,dl_fof2)
c      common /crun_directory/ run_directory
c         character run_directory*50
c      nch_run=lenchar(run_directory)

      open(lu,file=trim(root_directory)//PATH_SEPARATOR//'database'//PATH_SEPARATOR//'fof2var.txt',status='old')
      rewind(lu)
      read(lu,'(a)') alf    !  skip header record
      do 20 itab=1,18       !  18 tables
      read(lu,'(a)') alf    !  skip table title
      nch=lenchar(alf)
ccc      write(*,'(i2,1h=,a)') itab,alf(1:nch)
      do 10 lat=19,1,-1
10    read(lu,11) (var(it,lat,itab),it=1,24)
11    format(2x,24f5.2)
20    continue
      close(lu)
      ionce_fo=1     !  set flag to not read table again!!!!!
      return
      end
c------------------------------------------------------------
      subroutine get_fof2_var(xlt,xlat,month,ssn,dl_v,du_v)
c          This is ITU-R P.1239
c
c          given:
c                xlt   = local time
c                xlat  = latitude degrees (+=North, -=South)
c                month = month (1-12) in Northern hemisphere
c                ssn   = SunSpot number
c          return:
c                 dl_v = Lower decile of foF2 variability - Table 2
c                 du_v = Upper decile of foF2 variability - Table 3
c*************************************************************************
      common /C_fof2_var/ ionce_fo,dl_fof2(24,19,3,3),du_fof2(24,19,3,3)
c         dl = lower decile foF2 variability
c         du = upper decile foF2 variability
c         24 = 24 local time
c         19 = latitude   lat=(i-1)*5 degrees
c          3 = SSN  1=<50  2=50to100  3=>100
c          3 = season  1=winter(11,12,1,2)  2=equinox  3=summer(5,6,7,8)
c*************************************************************************
      data lu/71/
      if(ionce_fo.ne.1) then    !  read table in once
         call read_fof2_var(lu)
      end if

      iseason=1
      if(month.ge.3 .and. month.le.4) then
         iseason=2
      else if(month.ge.5 .and. month.le.8) then
         iseason=3
      else if(month.ge.9 .and. month.le.10) then
         iseason=2
      end if
      if(xlat.lt.0.) iseason=4-iseason !  switch season if Southern Hemisphere
      issn=2
      if(ssn.lt.50. ) issn=1      !  Low SSN
      if(ssn.gt.100.) issn=3      !  High SSN
      ylat=abs(xlat)
      lat=ylat/5. + 1.
      it1=xlt + 1.
      it2=it1+1
      if(it2.gt.24) it2=1
      dt=xlt-float(it1-1)
      dlat=(ylat - float((lat-1)*5))/5.
      dl_v=fof2_interp(dt,dlat,dl_fof2(it1,lat  ,issn,iseason),
     +                         dl_fof2(it1,lat+1,issn,iseason),
     +                         dl_fof2(it2,lat+1,issn,iseason),
     +                         dl_fof2(it2,lat  ,issn,iseason))
      du_v=fof2_interp(dt,dlat,du_fof2(it1,lat  ,issn,iseason),
     +                         du_fof2(it1,lat+1,issn,iseason),
     +                         du_fof2(it2,lat+1,issn,iseason),
     +                         du_fof2(it2,lat  ,issn,iseason))
      return
      end
c------------------------------------------------------------
      function fof2_interp(dx,dy,z1,z2,z3,z4)
      z14=z1 + (z4-z1)*dx
      z23=z2 + (z3-z2)*dx
      z=z14 + (z23-z14)*dy
ccc      write(*,1) dx,dy,z1,z2,z3,z4,z
ccc1     format('interp=',7f8.3)
      fof2_interp=z
      return
      end
c------------------------------------------------------------
