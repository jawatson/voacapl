!--------------------------------------------------------------
!# anttyp99.for
!***********************************************************************
PROGRAM anttyp99
!***********************************************************************
! Calculate an exteranl antenna file.
! Assumes data is in the format of:
!       ..\antennas\samples\sample.90
! Execute with:
!    anttyp99 directory mode
! where:
!    directory = full pathname to the RUN directory (e.g. c:\ITSHFBC\RUN)
!    mode      = (blank) = Point-to-Point
!              = a = Area Coverage
!***********************************************************************

    use Cant99
    
    implicit none

    real :: elev, freq, g ! <- Gain looks strange... what's it used for?
    real :: xfqs,xfqe,designfreq, aeff
    real ::beammain,offazim,cond,diel
    real, dimension(91) :: gain
    character(len=10) :: anttype
    character(len=70) :: antname
    character(len=24) :: antfile
    character (len=80) :: filename, gainfilename
    character (len=120) :: run_directory
    character (len=1) :: mode
    integer :: azimuth
    integer :: nch, nch_run, idx, iel, iazim, ifreq
    integer, parameter :: dat_file_un = 21
    integer, parameter :: gain_file_un = 22
    integer, parameter :: lua = 42
      
!...START OF PROGRAM
    call GET_COMMAND_ARGUMENT(1, run_directory)
    nch_run=len(trim(run_directory))
    if(nch_run.lt.3) go to 930
      
    call GET_COMMAND_ARGUMENT(2, mode)

    open(dat_file_un,file=run_directory(1:nch_run)//'/anttyp99.dat', status='old',err=900)
    rewind(dat_file_un)
    read(dat_file_un,*,err=920) idx          !  antenna index #, GAINxx.dat
    read(dat_file_un,'(a)',err=920) antfile  !  antenna file name
    read(dat_file_un,*,err=920) xfqs         !  starting frequency
    read(dat_file_un,*,err=920) xfqe         !  ending frequency
    read(dat_file_un,*,err=920) beammain     !  main beam (deg from North)
    read(dat_file_un,*,err=920) offazim      !  off azimuth (deg from North)
    close(dat_file_un)

    write (*,'(f8.3)') beammain
    write (*,'(f8.3)') offazim

    nch=len(trim(antfile))
    filename=run_directory(1:nch_run-3)//'antennas/'//antfile(1:nch)

    call ant99_read(filename,21,lua,*910)
    diel=parms(3)         !  dielectric constant
    cond=parms(4)         !  conductivity
    write(gainfilename,1) run_directory(1:nch_run),idx
1   format(a,5h/gain,i2.2,4h.dat)

    open(gain_file_un,file=gainfilename)
    rewind(gain_file_un)
    write(gain_file_un,'(a)') 'HARRIS99  '//title

    if(mode.eq.' ') then
!****************************************************************
!                  Point-to-Point mode
!****************************************************************
        write(gain_file_un,2) xfqs,xfqe,beammain,offazim,cond,diel
2       format(2f5.0,2f7.2,2f10.5)
        azimuth=offazim
        write(*, '(AI3)') '1. Off az = ', azimuth
        do ifreq=1,30
            freq=ifreq
            if(freq.ge.xfqs .and. freq.le.xfqe) then    !  in frequency range
                do iel=0,90
                    elev=iel
                    write(*, '(AI3)') '2. Off az = ', azimuth
                    call ant99_calc(freq,azimuth,elev,gain(iel+1),aeff,*940)
                end do
            else                                        !  outside freq range
                aeff=0.
                do iel=0,90
                    gain(iel+1)=0.
                end do
            end if
            write(gain_file_un,3) ifreq,aeff,gain
3           format(i2,f6.2,(T10,10F7.3))
        end do
    else

!****************************************************************
!                    Area Coverage mode
!****************************************************************
        write(gain_file_un,2) 2.0,xfqe,beammain,-999.,cond,diel
        freq=xfqs
        call ant99_calc(freq,0.,8.,g,aeff,*940)
        write(gain_file_un,201) freq,aeff
201     format(10x,f7.3,'MHz eff=',f10.3)
        do iazim=0,359
            azimuth=iazim
            do iel=0,90
                elev=iel
                call ant99_calc(freq,azimuth,elev,gain(iel+1),aeff,*940)
            end do
            write(gain_file_un,251) iazim,gain
251         format(i5,(T10,10F7.3))
        end do
    end if 
!****************************************************************
    call ant99_close      !Close the scratch file
    close(gain_file_un)
!****************************************************************
    go to 999
!****************************************************************
900 write(*,901) run_directory(1:nch_run)//'/anttyp99.dat'
901 format(' In anttyp99, could not OPEN file=',a)
    stop 'OPEN error in anttyp99 at 900'
910 write(*,911) filename
911 format(' In anttyp99, error READing file=',a)
    stop 'READ error in anttyp99 at 910'
920 write(*,921) run_directory(1:nch_run)//'/anttyp99.dat'
921 format(' In anttyp99, error READing file=',a)
    stop 'READ error in anttyp99 at 920'
!***********************************************************************
930 write(*,931)
931 format('anttyp99 must be executed:',&
        '1. Point-to-Point:',&
        '   anttyp99.exe run_directory',&
        '4. Area Coverage:',&
        '   icepacw.exe run_directory a')
    write(*,932)
932 format(&
        'Where:',&
        '      run_directory = full pathname to RUN directory',&
        '                      (e.g. /home/usr_name/itshfbc/run)')
    stop 'anttyp99 not executed properly.'
940 write(*,941) filename
941 format(' In anttyp99, error Calculating from file=',a)
    stop 'READ error in ANTTYP99 at 940'
!***********************************************************************
999 continue
end
