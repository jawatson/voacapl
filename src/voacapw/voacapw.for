c# voacap.f
c      winapp 80000,260000
ccc      winapp 240000,500000
      PROGRAM VOACAPW
*    +                (filein,fileout,areach)
C***********************************************************************
      use voacapl_defs
      use cversn
      use crun_directory
C***********************************************************************
      character(len=3), parameter :: COMPILER='w32'       !  32-bit compiler

c jw     include <windows.ins>
C***********************************************************************
c Execute with:
c    VOACAPW.EXE [S] directory VOACAPX.DAT VOACAPX.OUT a  (P-P circuit)
c    VOACAPW.EXE [S] directory VOACAPG.DAT VOACAPG.OUT    (P-P graph)
c    VOACAPW.EXE [S] directory VOACAPD.DAT VOACAPD.OUT    (P-P distance)
c    VOACAPW.EXE [S] directory VOACAPT.DAT VOACAPT.OUT    (P-P time)
c    VOACAPW.EXE [S] directory BATCH                      (P-P batch)
c    VOACAPW.EXE [S] directory BATCH deckname.dat         (P-P NEW batch)
c    VOACAPW.EXE [S] directory BATCH deckname.dat nam.out (P-P NEW batch)
c    VOACAPW.EXE [S] directory AREA CALC VOAAREAW.CIR     (AREA batch)
c    VOACAPW.EXE [S] directory AREA method pathname       (AREA single)
c    VOACAPW.EXE [S] directory INV  CALC VOAAREAW.CIR     (AREA INVERSE batch)
c    VOACAPW.EXE [S] directory INV  method pathname       (AREA INVERSE single)
c    VOACAPW.EXE [S] directory CIRAF pathname             (CIRAF single)
c where:
c    [S]       = SILENT, then no messages are written to output window
c    directory = full pathname to the install directory (e.g. c:\ITSHFBC)
c    method    = (CALC/SCREEN/PRINT)
c    pathname  = pathname below directory\AREADATA\ of input data file
c                (e.g. DEFAULT\DEFAULT.VOA)
c    deckname  = where raw input deck is found.
c                cannot be named VOACAPX.DAT.
c    a         = append results to output file
c
C***********************************************************************
C************************************************************************
c          Execute with:
c                  voacapw filein fileout list
c                       where:
c                             filein  = input card image data file
c                                     = 'batch' is special batch processing
c                             fileout = output file to create
c                             list    = n = do not list input deck
c                                     =   = list input deck
c                                     = a = area coverage mode
c                                           fileout is directory name
c                                           filein 1st record contains name
C***********************************************************************
c  Modification by Greg Hand NTIA/ITS.S3  (303)-497-3375.
c  VOACAP modified for NTIA/ITS HP730 computer system beginning 7/28/92.
c  The D in column 1 are lines which will NOT be compiled for PCVOACAP.
c  PCVOACAP is compiled with MicroSoft FORTRAN. This should simplify
c  by not needing to maintain 2 different sources. Some other minor
c  differences need to be addressed ( e.g. getting filein,fileout).
C--------------------------------
c%lc:gsp 28-DEC-1994:1st change, program voacapw   *********************
c%lc    .Changes made affecting "MUF DAYS" calculation in REGMOD
c%lc    .Changes made in reliability calculation in RELBIL
c%lc
c%lc    comments regarding changes may be collected by stripping lines
c%lc    with this 'c%lc' prefix. Should be insensitive to case to be safe.
c%lc            George Scott Pinson
c%lc            Lucas Consulting
c%lc            2900 Valmont Rd, Suite H
c%lc            Boulder, Co 80301
c%lc:. end change                  ***************************************
C***********************************************************************
C
C  THIS is the July 1988 Version of the IONOSPHERIC COMMUNICATIONS ANALYSIS
C  AND PREDICTION Program (IONCAP) developed by Frank RHOADS, NRL Code 4184,
C  from the 85.04 version obtained from I.T.S. by, AND for, VOA.  It is an
C  INTEGRATED SYSTEM of SUBROUTINEs that is designed to analyse IONOSPHERIC
C  PARAMETERs AND PREDICT the PERFORMANCE of RADIO SYSTEMs that utilize
C  HIGH-FREQUENCY (HF) SKY-WAVE PROPAGATION.
C
C---------------------------------
C
C  D     INCLUDE 'NEWCAP.DOC/LIST'      ! 3 additional PAGEs of INFO
C
C WEIGHTS AND ABSCISSCAE FOR 40 POINT GUASSIAN SET BELOW:
      COMMON / DATR /  WT(20), XT(20), NT, NPL, XNPL, TWDIV
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16,
     1                 LU20, LU25, LU26, LU35
C NUMERICAL MAP COEFICENTS (SEE REDMAP):
      COMMON / ONE /   IA(6), IB(6), IKIM(10,6), ESLCOF(5,55),
     1                 ESMCOF(7,61), ESUCOF(5,55), F2COF(13,76),
     2                 FM3COF(9,49), ERCOF(9,22)
C MORE COEFICENTS AND TABLES (SEE REDMAP):
      COMMON / TWO /   F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     1                 FAM(14,12), SYS(9,16,6), PERR(9,4,6)
      COMMON /TWO_AB/ AB(318)
      common /Cround/ iround
      common /Careach/ areach
         character areach*1
      character filein*64,fileout*64,areafile*30
      character file_batch*64
c jw      integer*2 istat
      integer istat
      common /cmodel/ model
      character model*8
      common /clisting/ listing,formfeed
      character listing*1,formfeed*1
      common /c_S_to_I/ i_S_to_I         !  =1= S/I calculations
      logical           :: iharris
      common /Charris/ iharris    !  =1=harris99.exe exists
      common /cdistance/ idistance,ndistance,ihr    !  plot vs distance
      common /ctime/ ntime                          !  plot vs time
      common /cCIRAF_TP/ nTP,idx_TP(911)
      character cmnam*64,title*80,ich*1,area_meth*1,dum*1
      character message*80,c_arg*50,PROGRAM*300
      logical*1 doesit
c jw      logical*4 fexists@
      logical*1 fexists
c jw      integer*4 window_handle,error_code
c jw      integer*2 x_pos,y_pos,xsize,ysize
C  PROGRAM VERSION NUMBER, PROGRAM CONTROL VARIABLES
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
c      common /CVERSN/ VERSN
c      character VERSN*8
      character(len=1) :: ABSORPTION_MODE=" "
c jw      real*8 start_time,end_time

c      common /crun_directory/ run_directory
c         character run_directory*50
      common /cQUIET/ iquiet
      character alf_narea_batch*4,alf_iarea_batch*4
      character alf_elapsed_time*8,alf_fileout*50
      common /Cprogress/ iarea_batch,alf_label
         character alf_label*80
      common /Ccancel_batch/ icancel_batch
      external cancel_batch
      character alf*80,append*1
c jw      integer (kind=3) count_underflow
c jw      logical (kind=2) permission

c *******************************************************************
c Integer added by JW to emulate the function of the cmnam() function
c using the get_command_argument() function
c *******************************************************************
      integer argCtr !jw added by JW to emulate the function of cmnam()
      integer fileNumCtr !jw File Number Counter used for nameing area files
      character(len=5)  :: ci
      character(len=256) :: cmd

C
C SET PREDEFINED CONSTANTS:
C
      DIMENSION IIA (6), IIB (6)
C
      DATA IIA / 1,276,703,978,1966,2407 /
      DATA IIB / 5, 7, 5, 13, 9, 9 /
C
      F2D(1,1,1) = -1.
      DO 100 I = 1, 6
      IA (I) = IIA (I)
 100  IB (I) = IIB (I)
C  DEFINE NT - POINT GAUSSIAN INTEGRATION
      NT = 20
C  USE LINEAR INTERVAL TRANSFORMATION TO (-1,1)
      NPL = 1
      XNPL = NPL
      TWDIV = 1. / (2. ** NPL)
c******************************************************
      i_S_to_I=0               !  =1= S/I calculations
      iarea_batch=0
      icancel_batch=0
      model='  VOACAP'
      nTP=0                    !  # of CIRAF test points to calculate
      ntime=0
      ihr=1
c******************************************************
c set the argument and file number counters to 1
c******************************************************
      argCtr = 1
      fileNumCtr = 1

c******************************************************
c jw      permission=.true.        !  ignore underflows
ccc      permission=.false.
c jw      call permit_underflow@(permission)
c******************************************************
c jw      run=cmnam()
      call get_command_argument(argCtr, c_arg) !jw
      argCtr = argCtr + 1
      nch=lenchar(c_arg)
c jw      if(nch.le.3) go to 930
c jw      call ucase(run,nch)
C******************************************************************
C Process posix type commands that appear before then run directory
C******************************************************************
      run_directory = ""
      area_directory = ""
      do command=1, COMMAND_ARGUMENT_COUNT()
        if (c_arg(1:1).ne.'-') exit

        if((c_arg(1:9).eq.'--version').or.(c_arg(1:2).eq.'-v')) then
           write(*,'(''voacapl - release '',a)') VOACAPL_VERSION
           call exit(0)
        else if (c_arg(1:2).eq.'-h') then
           call print_help()
           call exit(0)
        else if((c_arg(1:2).eq.'--silent').or.(c_arg(1:2).eq.'-s')) then
           iquiet=1
        else if(c_arg(1:18).eq.'--absorption-mode=') then
           if (scan("WIAa", c_arg(19:19))>0) then
               ABSORPTION_MODE=c_arg(19:19)
           else
               write(*, '(AA)') "Invalid absorption mode: ", c_arg(19:19)
           end if
        else if (c_arg(1:10).eq.'--run-dir=') then
            run_directory = c_arg(11:len(trim(c_arg)))
        else if (c_arg(1:11).eq.'--area-dir=') then
            area_directory = c_arg(12:len(trim(c_arg)))
        else
           write(*, '(AA)') "Option not recognised: ", c_arg
        end if

        call get_command_argument(argCtr, c_arg)
        argCtr = argCtr + 1

      end do

c******************************************************
c     check that the itshfbc directory exists, quit with
c     a message about creating one if not.
c******************************************************
c      nch=len(trim(c_arg))
      inquire(file=trim(c_arg)//'/.', exist=doesit)
      if (.not. doesit) goto 941

      root_directory=trim(c_arg)
      if (len(trim(run_directory)).eq.0) then
        run_directory=trim(root_directory)//PATH_SEPARATOR//'run'
      end if
      
      inquire(file=trim(run_directory)//'/.', exist=doesit)
      if (.not. doesit) goto 947
        
      call set_run            !  make sure we are in ..\RUN directory
      nch_run=lcount(run_directory,50)
ccc      open(72,file='voacap_dmp.txt')
ccc      rewind(72)
c******************************************************
      ierase=0    !  do not erase
      alf='ERASE debug window'
c jw      open(21,file=run_directory(1:nch_run-3)//'database\debug.txt',status='old',err=101)
      open(21,file=trim(root_directory)//PATH_SEPARATOR//'database'//PATH_SEPARATOR//'debug.txt',status='old',err=101)
      rewind(21)
      read(21,'(a)') alf
      close(21)
101   if(alf(1:18).eq.'ERASE debug window') ierase=1  !  do erase
ccc      ierase=0    !  do not erase
c******************************************************
      iround=0    !  do not round
ccc      open(21,file=run_directory(1:nch_run-3)//
ccc     +        'database\round.txt',status='old',err=102)
ccc      close(21)
ccc      iround=1  !  do round
ccc102   continue
c******************************************************
c jw      if(iquiet.eq.0) then
c jw         title='VOACAP output for:'
c jw         xsize=GetSystemMetrics(SM_CXSCREEN)
c jw         ysize=GetSystemMetrics(SM_CYSCREEN)/3
c jw         x_pos=0
c jw         y_pos=0
c jw         window_handle=create_window(title,x_pos,y_pos,xsize,ysize)
c jw         ier=set_default_window@(window_handle)
c jw      end if
c****************************************************************
      if(iquiet.eq.0) write(*,'('' Root Directory: '',a)') trim(root_directory)
      if(iquiet.eq.0) write(*,'('' Run Directory: '',a)') trim(run_directory)
      
c****************************************************************
c jw      iharris=it_exist(run_directory(1:nch_run-3)//bin_win\anttyp99.exe')
      inquire(file=trim(root_directory)//PATH_SEPARATOR//'bin_win'//PATH_SEPARATOR//'anttyp99.exe', exist=iharris)
c****************************************************************
c jw      call del_abt     !  delete the voaarea.abt & voacap.abt files
      listing='Y'
      areach=' '
c jw      filein=cmnam()
      call get_command_argument(argCtr, filein)
      argCtr = argCtr + 1 !jw
ccc      write(*,'('' filein='',a)') filein
      if(filein(1:1).eq.' ') filein='voacapx.dat'
c jw      call lcase(filein,20)
      if(filein(1:5).eq.'area ' .or. filein(1:4).eq.'inv ') then ! area coverage
         if (len(trim(area_directory))==0) then
            area_directory=trim(root_directory)//PATH_SEPARATOR//'areadata'
            area_inv_directory=trim(root_directory)//PATH_SEPARATOR//'area_inv'
         end if
         if(iquiet.eq.0) write(*,'('' Area Directory: '',a)') trim(area_directory)
         inquire(file=trim(area_directory)//'/.', exist=doesit)
         if (.not. doesit) goto 947
         areach='A'
         if(filein(1:1).eq.'i') areach='I'    !  inverse area coverage
c jw         area_meth=cmnam()
c jw         if(area_meth.eq.' ') area_meth='c'
         call get_command_argument(argCtr, area_meth) ! jw
         argCtr = argCtr + 1 !jw
         if(LEN_TRIM(area_meth) == 0) area_meth='c'
c jw         filein=cmnam()
         call get_command_argument(argCtr, filein) ! jw
         argCtr = argCtr + 1 !jw
c        Check the area input file exists and is readable
         inquire(file=trim(area_directory)//PATH_SEPARATOR//filein, exist=doesit)
         if(.NOT.doesit) goto 944
c jw         call lcase(filein,20)
ccc         write(*,'('' filein='',a)') filein
         if(filein(1:12).eq.'voaareaw.cir') then    !  batch area coverage
c jw            call seconds_since_1980@(start_time)    !  use to calc time
            iarea_batch=iarea_batch+1
c jw            open(61,file=run_directory(1:nch_run)//'\'//filein, status='old',err=920)
            open(61,file=trim(run_directory)//PATH_SEPARATOR//filein, status='old',err=920)
            rewind(61)
            call count_batch(61,narea_batch)  !  count # files to process
            read(61,'(a)',end=999) filein
            if(iquiet.eq.0) write(*,39) iarea_batch,narea_batch,filein
39          format(' BATCH area file(',i4,' of ',i4,'):',a)
            write(alf_narea_batch,'(i4)') narea_batch
            write(alf_iarea_batch,'(i4)') iarea_batch
c jw            call seconds_since_1980@(end_time)    !  use to calc time
c jw            elapsed=end_time-start_time
c jw            write(alf_elapsed_time,'(f8.1)') elapsed/60.
c jw            k=winio@('%ca[Batch VOACAP Area calculations progress]&')
c jw            alf_fileout=' '
c jw            k=winio@('fileout= %50st&',alf_fileout)
c jw            alf_label=' '
c jw            k=winio@('%nl%80st&',alf_label)
c jw            k=winio@('%nl%nl%cnCalculating file %tc[red]%4st %tc[black]'
c jw     +               //' of '//alf_narea_batch//'&',alf_iarea_batch)
c jw            k=winio@('%nl%cnElapsed time: %tc[red]%8st %tc[black]'//
c jw     +               'minutes&',alf_elapsed_time)
c jw            icancel_batch=0
c jw            k=winio@('%nl%nl%cn%^bt[&Cancel]&',cancel_batch)
c jw            k=winio@('%lw',iprocess_ctrl)      !  leave window open
         end if
         fileout='voaareax'
ccc         write(*,'(''before areamap, filein='',a)') filein
         call areamap(areach,filein,fileout,area_meth)
         filein='voaareax.da1'
c jw         fileout='..\AREADATA\'
         fileout=trim(area_directory)//PATH_SEPARATOR
c jw         if(areach.eq.'I') fileout='..\AREA_INV\'
         if(areach.eq.'I') fileout=trim(area_inv_directory)
      else if(filein(1:6).eq.'batch ') then     !  Batch point-to-point
         areach='B'
c jw         file_batch=cmnam()       !  is this "new" Special batch?
         call get_command_argument(argCtr, file_batch) !  is this "new" Special batch?
         argCtr = argCtr + 1 !jw
         if(file_batch(1:1).ne.' ') areach='S'    !  YES!!!
         filein='voacapx.dat'
c jw         fileout=cmnam()
         call get_command_argument(argCtr, fileout) !jw
         argCtr = argCtr + 1 !jw
         if(fileout(1:1).eq.' ') fileout='voacapb.out'
         nch_out=lcount(fileout,64)
c jw         call erase@(run_directory(1:nch_run)//'\'//fileout(1:nch_out),istat)   !  delete any previous file
         call unlink(trim(run_directory)//PATH_SEPARATOR//fileout(1:nch_out), istat)
ccc         write(*,'('' after erase, istat='',i5)') istat
c jw         if(istat.ne.0 .and. iquiet.eq.0) then
c jw            call dos_error_message@(istat,message)
c TODO insert an error message here.
ccc            write(*,'('' istat='',i5,1h=,a)') istat,message
ccc            write(*,'('' file='',a)') run_directory(1:nch_run)//
ccc     +                                '\'//fileout(1:nch_out)
c jw         end if
      else
c jw         fileout=cmnam()
         call get_command_argument(argCtr, fileout) !jw
         argCtr = argCtr + 1
         if(fileout(1:1).eq.' ') fileout='voacapx.out'
c jw         call lcase(fileout,20)
      end if
c jw      append=cmnam()       !  should we append to output file
      call get_command_argument(argCtr, append) !jw
      argCtr = argCtr + 1 !jw
      if(append.eq.'A') append='a'
      listing=areach
      if(listing.eq.'n') listing='N'
      if(listing.eq.'a') listing='A'
      if(filein (1:1).eq.' ' .or. fileout(1:1).eq.' ')
     +      stop 'Execute with: voacapw filein fileout'
      iabort=0
c jw      call del_abt      !  delete the voacap.abt & voaarea.abt files
c******************************************************
C
C.....REWIND INPUT LOGICAL UNITS BEFORE EXECUTION:
C
C                THIS IS THE USER-GENERATED INPUT FILE
40    continue
ccc      write(*,'(''after 40, area='',a)') areach
      if(areach.eq.'A' .or. areach.eq.'I') then  !  get real file name of output
         open(LU5,file=trim(run_directory)//PATH_SEPARATOR//filein,STATUS='OLD',iostat=ios,err=942)
         rewind(lu5)
         read(lu5,'(20x,a)') areafile
         close(lu5)
         write(*,'(''fileout='',a)') fileout
         write(*,'(''areafile='',a)') areafile
         write(*,'(''filein='',a)') filein
         nch=len(trim(fileout))
         nch2=len(trim(areafile))
         nch3=len(trim(filein))
         areafile(nch2-2:nch2)='vg'//filein(nch3:nch3)
         fileout(nch+1:nch+nch2)=areafile(1:nch2)
         write(*, '(A)') fileout
      else if(listing.eq.'B') then           !  Initialize batch processing
         call read_asc('VOACAP',*999)  !read pt-to-pt common from VOACAPW.ASC
         open(38,file=trim(run_directory)//PATH_SEPARATOR//'voacap.cir',
     +        status='old',err=999)
         rewind(38)
         read(38,'(a)',err=999) dum     !  skip 1st record
         if(iquiet.eq.0) then
             write(*,'('' Output is being written to: '',a,/)') trim(fileout)
         end if
         icircuit=0
         call batch(38,'VOACAP',filein,icircuit,*999)
      else if(listing.eq.'S') then           !  New SPECIAL batch
         nch_batch=lcount(file_batch,64)
         open(38,file=trim(run_directory)//PATH_SEPARATOR//file_batch(1:nch_batch),status='old',err=999)
         if(iquiet.eq.0)
     +   write(*,'('' Output is being written to: '',a,/)')
     +                 fileout(1:nch_out)
         icircuit=0
         call batch_S(38,'VOACAP',filein,icircuit,*999)
      end if
c***********************************************************
ccc50    inquire(file=filein,exist=doesit)   !  if file does not exist, quit
50    nch_in=lcount(filein,64)
c jw      doesit=fexists@(
c jw     +     run_directory(1:nch_run)//'\'//filein(1:nch_in),
c jw     +     error_code)   !  if file does not exist, quit
ccc      write(*,'(''opening file='',a)') filein
      inquire(file=trim(run_directory)//PATH_SEPARATOR//filein,exist=doesit)
      if(.NOT.doesit) go to 950
      if(iquiet.eq.0) then
         if(filein(1:7).eq.'voacapw') write(*,51) 'WANTED  '
         if(filein(1:7).eq.'voacapu') write(*,51) 'UNWANTED'
51       format(' Calculating ',a,' signal')
      end if
      call antcalc(filein,listing)
c***********************************************************
      if(iquiet.eq.0) then
         if(listing.eq.'A') then
            write(*,'('' Area filein ='',a)') filein
            write(*,'('' fileout='',a)') fileout
            if(iarea_batch.ne.0) then
               alf_fileout=fileout
c jw               call window_update@(alf_fileout)
            end if
         else if(listing.eq.'I') then
            write(*,'('' Inverse Area filein ='',a)') filein
         end if
      end if
c***********************************************************
ccc      write(*,'(''listing, areach='',a,1h:,a)') listing,areach
ccc      write(*,'(''opening file='',a)') filein
      open(LU5,file=trim(run_directory)//PATH_SEPARATOR//filein,STATUS='OLD', iostat=ios,err=944)
      rewind(lu5)
      ndistance=1
      if(fileout(1:11).eq.'VOACAPD.OUT' .or.
     +   fileout(1:11).eq.'voacapd.out') ndistance=51  !  plot vs distance
      ntime=0
      if(fileout(1:11).eq.'VOACAPT.OUT' .or.
     +   fileout(1:11).eq.'voacapt.out') ntime=1       !  plot vs time
      if(areach.eq.'B' .or. areach.eq.'S') then        !  batch, use APPEND
c jw         open(LU6,file=run_directory(1:nch_run)//'\'//fileout, status='APPEND')
         open(LU6,file=trim(run_directory)//PATH_SEPARATOR//fileout, access='APPEND')
c jw         formfeed=''
         formfeed='\n\f'
      else if(fileout(1:2).eq.'..') then
         nchf=lcount(fileout,64)
ccc         write(*,'('' opening area file='',a)')
ccc     +                 run_directory(1:nch_run-3)//fileout(4:nchf)
ccc         write(*,'(''areafile='',a)') areafile
         open(LU6,file=trim(root_directory)//fileout(4:nchf), iostat=ios,err=946)
         rewind(lu6)
         formfeed=' '
      else if(append.eq.'a') then
c jw         open(LU6,file=run_directory(1:nch_run)//'\'//fileout,status='APPEND')
         open(LU6,file=trim(run_directory)//PATH_SEPARATOR//fileout, access='APPEND')
c jw         formfeed=''
         formfeed='\n\f'
ccc         write(*,'('' file opened OK'')')
      else
ccc         write(*,'('' opening file='',a)')
ccc     +                 run_directory(1:nch_run)//'\'//fileout
         write(*, '(''opeing file'',a)') trim(run_directory)//PATH_SEPARATOR//fileout
         open(LU6,file=trim(run_directory)//PATH_SEPARATOR//fileout, iostat=ios,err=948)
         rewind(lu6)
         formfeed=' '
      end if
ccc      write(*,'('' opening LU15/scratch'')')
cccc      open(LU15,status='scratch')
      open(LU15,file=trim(run_directory)//PATH_SEPARATOR//'lu15_voa.tmp', iostat=ios,err=960)
ccc      write(*,'('' opening LU35/scratch'')')
cccc      open(LU35,status='scratch')
      open(LU35,file=trim(run_directory)//PATH_SEPARATOR//'lu35_voa.tmp', iostat=ios,err=962)
      REWIND LU15
      REWIND LU35
c***********************************************************
      if(ndistance.gt.1) then
         OPEN(48,file=trim(run_directory)//PATH_SEPARATOR//'voacapd.idx')
         rewind(48)
c jw         call erase@(run_directory(1:nch_run)//'\VOACAPD.DST',istat)
         call unlink(trim(run_directory)//PATH_SEPARATOR//'voacapd.dst',istat)
         OPEN(49,file=trim(run_directory)//PATH_SEPARATOR//'voacapd.dst',
     +        access='direct',form='unformatted',recl=108)
      end if
      if(ntime.ne.0) then
         OPEN(48,file=trim(run_directory)//PATH_SEPARATOR//'voacapt.idx')
         rewind(48)
c jw         call erase@(run_directory(1:nch_run)//'\VOACAPT.DST',istat)
         call unlink(trim(run_directory)//PATH_SEPARATOR//'voacap.dst',istat)
         OPEN(49,file=trim(run_directory)//PATH_SEPARATOR//'voacap.dst',
     +        access='direct',form='unformatted',recl=96)
      end if
c***********************************************************
      open(21,file=trim(root_directory)//PATH_SEPARATOR//'database'//PATH_SEPARATOR//'version.'//COMPILER,status='old',iostat=ios,err=964)
      rewind(21)
      read(21,'(8x,a)') VERSN
c.....Modify the VERSN string if we have a user defined mode.
      if (len(trim(ABSORPTION_MODE))>0) then
          VERSN = VERSN(1:7)//ABSORPTION_MODE
      end if

      close(21)
ccc      write(*,'('' version='',a)') VERSN
c****************************************************************
      call set_magnetic_pole    !  get the location of the geomagnetic north pole
c****************************************************************
      iabort=1
      if(ndistance.gt.1 .or. ntime.ne.0) then
         CALL HFMUFS2(fileout,*950)           !  distance or time calculations
      else
         CALL HFMUFS(fileout,*950)            !  normal
      end if
      iabort=0          !  was not aborted
      if(i_S_to_I.ne.0) close(18)     !  close special S/I output file
      CLOSE(LU15)
      CLOSE(LU35)
      CLOSE(LU5)
      close(lu6)
c*********************************************************************
c          do we need to PLOT the results?
c*********************************************************************
c jw      if((listing.eq.'A' .or. listing.eq.'I') .and.
c jw     +    (area_meth.eq.'p'.or.area_meth.eq.'s'))then
c jw         if(iquiet.eq.0) write(*,'('' Plotting:'',a)') fileout
c jw         nchf=lcount(fileout,64)
c jw         PROGRAM=run_directory(1:nch_run-3)//
c jw     +             'bin_win\worldwin.exe '//
c jw     +       run_directory(1:nch_run)//' '//fileout(4:11)//' '//
c jw     +       fileout(13:nchf)//' '//
c jw     +              area_meth
c jw         nch=lcount(PROGRAM,300)
c jw         call gh_exec(PROGRAM,nch,1)   !  execute and wait for WORLDWIN.EXE
c jw      end if

c**************************************************************
c The following section has been modified from the original to
c permit longer file extensions to be used (e.g. 'da12')
c**************************************************************
      if(listing.eq.'A' .or. listing.eq.'I') then   !area coverage, are there more files to process
         ich=filein(nch3:nch3)
         fileNumCtr = fileNumCtr+1
         if (fileNumCtr.eq.(MAX_AREA_MONTHS+1)) then
            go to 950
         else                    !  process next file in order
            if (fileNumCtr <= 9) then
                write (filein,'(A11I1)') "voaareax.da", fileNumCtr
                write (fileout, '(AI1)') fileout(1:index(fileout, '.vg')+2),fileNumCtr
            else
                write (filein,'(A11I2)') "voaareax.da", fileNumCtr
                write (fileout, '(AI2)') fileout(1:index(fileout, '.vg')+2),fileNumCtr
            endif
            filein = trim(filein)
            fileout = trim(fileout)
            go to 50                 !  calculate another area file
         end if
      else if(listing.eq.'B') then   !Batch point-to-point, next circuit
         call batch(38,'VOACAP',filein,icircuit,*999)
         go to 50                !  process next batch circuit
      else if(listing.eq.'S') then   !New Batch point-to-point, next circuit
         call batch_S(38,'VOACAP',filein,icircuit,*999)
         go to 50                !  process next batch circuit
      else
      end if
c***********************************************************************
      if(filein(1:12).eq.'voacapwx.dat' .or.
     +   filein(1:12).eq.'voacapwg.dat') then   !  need to process UNWANTED
         filein(7:7)='u'
         fileout(7:7)='u'
         go to 50
      end if
c***********************************************************************
      go to 950
c***********************************************************************
c*****voaarea.cir batch area coverage file not found
920   write(*,'('' Error: Could not open file voaarea.cir for BATCH AREA: '',a)') trim(run_directory)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:920'')')
      call exit(1) ! Exit if we can't find the right file.

c*****This error is no longer called
940   write(*,'('' Error: voacapl not executed properly.'')')
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')941
      call exit(1)

c*****itshfbc directory not found
941   write(*,'('' Error: Unable to open itshfbc directory at: '',a)') trim(root_directory)
      write(*,'('' Run the command "makeitshbc" to create a copy of the'')')
      write(*,'('' itshfbc directory in your home directory.'')')
      write(*, '('' VOACAPW:941'')')
      call exit(1) ! Exit if we can't find the right file.

c*****P2P prediction input file not found
942   write(*,'('' Error: The specified input file was not found: '',a)') trim(run_directory)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:942'')')
c      call exit(1) ! Exit if we can't find the right file.
      goto 950

c*****Area prediction input file not found
943   write(*,'('' Error: The specified input file was not found: '',a)') run_directory(1:nch_run)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:943'')')
      call exit(1) ! Exit if we can't find the right file.

c*****Missing or unreadable area data input file
944   write(*,'('' Error: The specified input file was not found: '',a)') '../areadata/'//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:'')')
      call exit(1) ! Exit if we can't find the right file.

c*****Missing or unreadable area data input file
945   write(*,'('' Error: The specified input file was not found: '',a)') run_directory(1:nch_run)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:945'')')
      call exit(1) ! Exit if we can't find the right file.

c*****Error opening output file
946   write(*,'('' Error opening output file : '',a)') trim(root_directory)//fileout(4:nchf)
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:946'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(1) ! Exit if we can't find the right file.

c*****itshfbc directory not found
947   write(*,'('' Error: Unable to open run directory at: '',a)') trim(run_directory)
      write(*, '('' VOACAPW:947'')')
      call exit(1) ! Exit if we can't find the run directory.


c*****Error opening output file
948   write(*,'('' Error opening output file : '',a)') fileout
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:948'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(1) ! Exit if we can't find the right file.

c****Error opening the scratch file
960   write(*,'('' Error opening temporary scratch file : '',a)') trim(run_directory)//PATH_SEPARATOR//'lu15_voa.tmp'
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:960'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(1) ! Exit if we can't find the right file.

c****Error opening the scratch file
962   write(*,'('' Error opening file : '',a)') trim(run_directory)//PATH_SEPARATOR//'lu35_voa.tmp'
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:962'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(1) ! Exit if we can't find the right file.

c****Error opening the vesion file
964   write(*,'('' Error opening file : '',a)') trim(root_directory)//PATH_SEPARATOR//'database'//PATH_SEPARATOR//'version.'//COMPILER
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:964'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(1) ! Exit if we can't find the right file.

c******************************************************************
950   CLOSE(LU15)
      CLOSE(LU35)
      if(icancel_batch.ne.0) go to 999
      if(iabort.ne.0) go to 999
      if(iarea_batch.eq.0) go to 999      !  not area coverage batch run
         read(61,'(a)',end=999) filein
         iarea_batch=iarea_batch+1
         if(iquiet.eq.0) write(*,39) iarea_batch,narea_batch,filein
         write(alf_iarea_batch,'(i4)') iarea_batch
c jw         call window_update@(alf_iarea_batch)
         write(alf_iarea_batch,'(i4)') iarea_batch
c jw         call seconds_since_1980@(end_time)    !  use to calc time
c jw         elapsed=end_time-start_time
c jw         write(alf_elapsed_time,'(f8.1)') elapsed/60.
c jw         call window_update@(alf_elapsed_time)
         fileout='voaareax'
ccc         write(*,'(''BEFORE areamap, filein='',a)') filein
ccc         write(*,'(''areach='',a)') areach
         call areamap(areach,filein,fileout,area_meth)
ccc         areach=area
         filein='voaareax.da1'
         fileout=trim(area_directory)//PATH_SEPARATOR
c TODO in the following line we should be using area_inv as the area directory
         if(areach.eq.'I') fileout=trim(area_directory)//PATH_SEPARATOR
         go to 40      !  begin next area calculation
999   continue
      if(iarea_batch.ne.0) then      !  batch area finish
         idone=iarea_batch           !  number of files done
         if(iabort.ne.0 .or. icancel_batch.ne.0) then
            if(iquiet.eq.0) write(*,'(1x)')
            idone=idone-1            !  current one was not done
         end if
         iprocess_ctrl=0
c jw         call window_update@(iprocess_ctrl)    !  remove process message
c jw         call seconds_since_1980@(end_time)    !  use to calc time
c jw         elapsed=end_time-start_time
         if(iquiet.eq.0) write(*,997) idone,elapsed/60.
997      format(50(1h*),/,
     +          i5,' BATCH Area calculations done.',f10.1,' minutes',/,
     +          50(1h*))
      end if
      if(listing.ne.'B' .and. iarea_batch.eq.0)then!don't destroy BATCH output
c jw         if(ierase.ne.0) call destroy_window(window_handle)
      else
         if(iabort.ne.0) write(*,998)
998                      format(/,' Batch processing has been aborted.')
      end if
c******************************************************************
c         close and delete temporary files
      close(LU15)
      close(LU35)
c jw      call erase@(run_directory(1:nch_run)//'\'//'LU15_VOA.TMP',istat)
      call unlink(run_directory(1:nch_run)//PATH_SEPARATOR//'lu15_voa.tmp', istat) ! jw
c jw      call erase@(run_directory(1:nch_run)//'\'//'LU35_VOA.TMP',istat)
      call unlink(run_directory(1:nch_run)//PATH_SEPARATOR//'lu35_voa.tmp', istat) ! jw
c jw      call underflow_count@(count_underflow)  !  see if any underflows occured
ccc      write(*,'(''underflow='',i8)') count_underflow
      END
c
c--------------------------------------------------------------
c###set_magnetic_pole.for
      SUBROUTINE set_magnetic_pole
c         This sets the location of the geomagnetic north pole
c         The default is (78.5N, 69.0W) = (78.5, -69.0)
      use voacapl_defs
      use crun_directory
      common /Cnorth_pole/ g_magnetic_lat,g_magnetic_lon  !  magnetic north pole
c      common /crun_directory/ run_directory
c         character run_directory*50
      data lu/21/

      nch_run=lcount(run_directory,50)
c***********************************************************
c          read the user values from ..\run\north_pole.txt
      open(lu,file=trim(run_directory)//PATH_SEPARATOR//'north_pole.txt',status='old',err=200)
      rewind(lu)
      read(lu,*,err=150) glat,glon
      close(lu)
      if(glat.lt.60. .or. glat.gt.90.) go to 200       !  bad values of latitude
      if(glon.lt.-180. .or. glon.gt.180.) go to 200    !  bad values of longitude
      go to 500     !  use these values
c***********************************************************
150   close(lu)
c          if user values do not exist,
c          read the default values from ..\database\north_pole.txt
200   open(lu,file=trim(root_directory)//'database'//PATH_SEPARATOR//'north_pole.txt',status='old',err=300)
      rewind(lu)
      read(lu,*,err=250) glat,glon
      close(lu)
      if(glat.lt.60. .or. glat.gt.90.) go to 300       !  bad values of latitude
      if(glon.lt.-180. .or. glon.gt.180.) go to 300    !  bad values of longitude
      go to 500     !  use these values
c***********************************************************
250   close(lu)
300   glat=78.5           !  set default values if data files do not exist
      glon=-69.0
500   g_magnetic_lat=glat
      g_magnetic_lon=glon
ccc      open(lu,file=run_directory(1:nch_run)//'\pole.txt')
ccc      write(lu,1) g_magnetic_lat,g_magnetic_lon
ccc1     format('Magnetic North =',2f10.3)
ccc      close(lu)
      return
      end
c----------------------------------------------------------------------

c----------------------------------------------------------------------

      subroutine print_help()
      print *, ' Usage:'
      print *, ' voacapl [-s] voacap_directory [inputfile]'
      print *, ' voacapl [-s] voacap_directory [inputfile outputfile]'
      print *, ' voacapl [-s] voacap_directory area calc [areafile]'
      print *, ' voacapl [-s] voacap_directory batch'
      print *, ' voacapl -v (prints the version number)'
      print *, ' voacapl -h (prints this help message)'
      end

