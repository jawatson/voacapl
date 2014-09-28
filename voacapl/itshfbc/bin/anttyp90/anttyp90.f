c--------------------------------------------------------------
c# anttyp90.for
C***********************************************************************
      PROGRAM anttyp90
*    +               (run_directory,mode)
C***********************************************************************
c Calculate an exteranl antenna file.
c Assumes data is in the format of:
c       ..\antennas\samples\sample.90
c Execute with:
c    anttyp90 directory mode
c where:
c    directory = full pathname to the RUN directory (e.g. c:\ITSHFBC\RUN)
c    mode      = (blank) = Point-to-Point
c              = a = Area Coverage
C***********************************************************************
      common /Cant90/ luaa,filenam,title,itype,parms(20),
     +                 nfreq,frequency(100),dbi(100),eff(100),
     +                 ifreq1,gain1(91,360),ifreq2,gain2(91,360)
      character filenam*80,title*80
      common /cantenna/ anttype,antname,
     +                  xfqs,xfqe,designfreq,antfile,
     +                  beammain,offazim,cond,diel,
     +                  gain(91)
      character anttype*10,antname*70,antfile*24

      character (len=80) :: filename, gainfile

      character (len=50) :: run_directory
      character (len=1) :: mode
      
C.....START OF PROGRAM
      call GET_COMMAND_ARGUMENT(1, run_directory) !jw
      write(*,'('' Run Directory      : '',a)') trim(run_directory)
      nch_run=len(trim(run_directory))
      if(nch_run.lt.3) go to 930
      
      call GET_COMMAND_ARGUMENT(2, mode)
      write(*,'('' Mode arg           : '',a)') mode

      open(21,file=run_directory(1:nch_run)//'/anttyp90.dat', status='old',err=900)
      rewind(21)
      read(21,*,err=920) idx          !  antenna index #, GAINxx.dat
      read(21,'(a)',err=920) antfile  !  antenna file name
      read(21,*,err=920) xfqs         !  starting frequency
      read(21,*,err=920) xfqe         !  ending frequency
      read(21,*,err=920) beammain     !  main beam (deg from North)
      read(21,*,err=920) offazim      !  off azimuth (deg from North)
      close(21)
      nch=len(trim(antfile))
      filename=run_directory(1:nch_run-3)//'antennas/'//antfile(1:nch)

      lua=42
      call ant90_read(filename,21,lua,*910)
      diel=parms(3)         !  dielectric constant
      cond=parms(4)         !  conductivity
      write(gainfile,1) run_directory(1:nch_run),idx
1     format(a,5h/gain,i2.2,4h.dat)
      open(22,file=gainfile)
      rewind(22)
      write(22,'(a)') 'Extern#90/'//title
c****************************************************************
      if(mode.ne.' ') go to 200     !  area coverage
c*****Point-to-Point mode
      write(22,2) xfqs,xfqe,beammain,offazim,cond,diel
2     format(2f5.0,2f7.2,2f10.5)
      azimuth=offazim
      do 50 ifreq=1,30
      freq=ifreq
      if(freq.ge.xfqs .and. freq.le.xfqe) then    !  in frequency range
         do 20 iel=0,90
         elev=iel
20       call ant90_calc(freq,azimuth,elev,gain(iel+1),aeff,*940)
      else                                        !  outside freq range
         aeff=0.
         do 30 iel=0,90
30       gain(iel+1)=0.
      end if
      write(22,3) ifreq,aeff,gain
3     format(i2,f6.2,(T10,10F7.3))
50    continue
      go to 500
c****************************************************************
c*****Area Coverage mode
200   write(22,2) 2.0,xfqe,beammain,-999.,cond,diel
      freq=xfqs
      call ant90_calc(freq,0.,8.,g,aeff,*940)
      write(22,201) freq,aeff
201   format(10x,f7.3,'MHz eff=',f10.3)
      do 250 iazim=0,359
      azimuth=iazim
      do 220 iel=0,90
      elev=iel
220   call ant90_calc(freq,azimuth,elev,gain(iel+1),aeff,*940)
250   write(22,251) iazim,gain
251   format(i5,(T10,10F7.3))
c****************************************************************
500   call ant90_close
      close(22)
c****************************************************************
      go to 999
c****************************************************************
900   write(*,901) run_directory(1:nch_run)//'/anttyp90.dat' !jw
901   format(' In anttyp90, could not OPEN file=',a)
      stop 'OPEN error in anttyp90 at 900'
910   write(*,911) filename
911   format(' In anttyp90, error READing file=',a)
      stop 'READ error in anttyp90 at 910'
920   write(*,921) run_directory(1:nch_run)//'/anttyp90.dat' !jw
921   format(' In anttyp90, error READing file=',a)
      stop 'READ error in anttyp90 at 920'
c***********************************************************************
930   write(*,931)
931   format('anttyp90 must be executed:',/
     +  '1. Point-to-Point:',/
     +  '   anttyp90.exe run_directory',/,
     +  '4. Area Coverage:',/
     +  '   icepacw.exe run_directory a')
      write(*,932)
932   format(/
     +  'Where:',/
     +  '      run_directory = full pathname to RUN directory',/
     +  '                      (e.g. /home/usr_name/itshfbc/run)')
      stop 'anttyp90 not executed properly.'
940   write(*,941) filename
941   format(' In anttyp90, error Calculating from file=',a)
      stop 'READ error in ANTTYP90 at 940'
c***********************************************************************
999   continue
      end
