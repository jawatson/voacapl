c#include "voa_defs.hdr"

      module verbose_mod
            real :: iquiet=0
            save
      end module verbose_mod

      module version_mod
c            common /CVERSN/ VERSN
            character(len=8)  :: VERSN
            save
      end module version_mod

      program VOACAPW
*    +(filein,fileout,areach)
      use error_codes
      use voa_defs_mod
      use verbose_mod
      use version_mod

      character(len=3), parameter :: COMPILER='w32'       !  32-bit compiler

      character(len=1), parameter :: PATH_SEPARATOR ='/'


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
c    directory = full pathname to the install directory (e.g. ~/itshfbc)
c    method    = (CALC/SCREEN/PRINT)
c    pathname  = pathname below directory /areadata/ of input data file
c                (e.g. DEFAULT/DEFAULT.VOA)
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
      common / DATR /  WT(20), XT(20), NT, NPL, XNPL, TWDIV
      common / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16,
     1                 LU20, LU25, LU26, LU35
C NUMERICAL MAP COEFICENTS (SEE REDMAP):
      common / ONE /   IA(6), IB(6), IKIM(10,6), ESLCOF(5,55),
     1                 ESMCOF(7,61), ESUCOF(5,55), F2COF(13,76),
     2                 FM3COF(9,49), ERCOF(9,22)
C MORE COEFICENTS AND TABLES (SEE REDMAP):
      common / TWO /   F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     1                 FAM(14,12), SYS(9,16,6), PERR(9,4,6)
      common /TWO_AB/ AB(318)
      common /Cround/ iround
      common /Careach/ areach
      character(len=1)  :: areach
      character(len=64) :: filein, fileout, file_batch
      character(len=30) :: areafile
      integer           :: istat

      common /cmodel/ model
      character(len=8)  :: model

      common /clisting/ listing,formfeed
      character(len=1)  :: listing, formfeed

      common /c_S_to_I/ i_S_to_I         !  =1= S/I calculations
      logical           :: iharris
      common /Charris/ iharris    !  =1=harris99.exe exists
      common /cdistance/ idistance,ndistance,ihr    !  plot vs distance
      common /ctime/ ntime                          !  plot vs time
      common /cCIRAF_TP/ nTP,idx_TP(911)

      character(len=64) :: cmname
      character(len=80) :: title
      character(len=1)  :: ich, area_meth, dum

      character(len=50) :: run

      logical           :: doesit = .false.
C  PROGRAM VERSION NUMBER, PROGRAM CONTROL VARIABLES
      common / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
c      common /CVERSN/ VERSN
c      character(len=8)  :: VERSN
      real              :: start_time, end_time

      common /crun_directory/ run_directory
      character(len=50) :: run_directory

c      common /cQUIET/ iquiet
c      use verbose_mod
      character(len=4)  :: alf_narea_batch, alf_iarea_batch
      character(len=8)  :: alf_elapsed_time
      character(len=50) :: alf_fileout

      common /Cprogress/ iarea_batch,alf_label
      character(len=80) :: alf_label, alf
      character(len=1)  ::append

c *******************************************************************
c Integer added by JW to emulate the function of the cmnam() function
c using the get_command_argument() function
c *******************************************************************
      integer argCtr !jw added by JW to emulate the function of cmnam()
      integer fileNumCtr !jw File Number Counter used for naming area files
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
c      icancel_batch=0
      model='  VOACAP'
      nTP=0                    !  # of CIRAF test points to calculate
      ntime=0
      ihr=1
c******************************************************
c set the argument and file number counters to 1
c******************************************************
      argCtr = 1
      fileNumCtr = 1
c      f_action = ''

c******************************************************
c      permission=.true.        !  ignore underflows
ccc      permission=.false.
c      call permit_underflow@(permission)
c******************************************************
      call cpu_time(start_time)
      call get_command_argument(argCtr, run) !jw
      argCtr = argCtr + 1
      nch=lenchar(run)
c      if(nch.le.3) go to 940
c      call ucase(run,nch)
      if(run(1:2).eq.'-v') then
         write(*,'(''voacapl - release '',a)') VOACAPL_VERSION
         call exit(EC_OK)
      else if (run(1:2).eq.'-h') then
         call print_help()
         call exit(EC_OK)
      end if
      iquiet=0
      if(run(1:2).eq.'-s') then
         iquiet=1
         call get_command_argument(argCtr, run) 
         argCtr = argCtr + 1   
         nch=lenchar(run)
         if(nch.le.3) go to 940
      end if
      
c******************************************************
c     check that the itshfbc directory exists, quit with 
c     a message about creating one if not.
c******************************************************
      inquire(file=run(1:len(trim(run)))//'/.', exist=doesit)
      if (.not. doesit) goto 941

      run_directory=run(1:nch)//PATH_SEPARATOR//'run'
      call set_run            !  make sure we are in ../run directory
      nch_run=lcount(run_directory,50)

      iround=0    !  do not round

      if (iquiet.eq.0) write(*,'('' Run Directory      : '',a)') run_directory(1:nch_run)
c****************************************************************
c      iharris=it_exist(run_directory(1:nch_run-3)//'bin_win/anttyp99.exe')
      inquire(file=run_directory(1:nch_run-3)//'bin_win'//PATH_SEPARATOR//'anttyp99.exe', exist=iharris)
c      The Harris code is not available so the check is pointless...
      iharris = .false.
c****************************************************************
c      call del_abt     !  delete the voaarea.abt & voacap.abt files
      listing='Y'
      areach=' '
      call get_command_argument(argCtr, filein) 
      argCtr = argCtr + 1 !jw
      if(filein(1:1).eq.' ') filein='voacapx.dat'
c todo check voacapx.dat exists
c      call lcase(filein,20)
      if(filein(1:5).eq.'area ' .or. filein(1:4).eq.'inv ') then ! area coverage
         areach='A'
         if(filein(1:1).eq.'i') areach='I'    !  inverse area coverage
         call get_command_argument(argCtr, area_meth) ! jw
         argCtr = argCtr + 1 !jw
         if(LEN_TRIM(area_meth) == 0) area_meth='c'
         call get_command_argument(argCtr, filein) ! jw
         argCtr = argCtr + 1 !jw
c        Check the area input file exists and is readable
         inquire(file='../areadata/'//filein, exist=doesit)
         if(.NOT.doesit) goto 944 

c         call lcase(filein,20)
         if(filein(1:12).eq.'voaareaw.cir') then    !  batch area coverage
c            call cpu_time(start_time)    !  use to calc time
            iarea_batch=iarea_batch+1
            open(61,file=run_directory(1:nch_run)//PATH_SEPARATOR//filein, status='old',err=920)
            rewind(61)
            call count_batch(61,narea_batch)  !  count # files to process
            read(61,'(a)',end=999) filein
            if(iquiet.eq.0) write(*,39) iarea_batch,narea_batch,filein
39          format(' BATCH area file(',i4,' of ',i4,'):',a)
            write(alf_narea_batch,'(i4)') narea_batch
            write(alf_iarea_batch,'(i4)') iarea_batch
c            call cpu_time(end_time)    !  use to calc time
c            elapsed=end_time-start_time
            write(*,'(f8.1)') elapsed/60.

         end if
         fileout='voaareax'
ccc         write(*,'(''before areamap, filein='',a)') filein
         call areamap(areach,filein,fileout,area_meth)
         filein='voaareax.da1'
         fileout='../areadata/'
         if(areach.eq.'I') fileout='../area_inv/'
      else if(filein(1:6).eq.'batch ') then     !  Batch point-to-point
         areach='B'
         call get_command_argument(argCtr, file_batch) !  is this "new" Special batch?
         argCtr = argCtr + 1 !jw
         if(file_batch(1:1).ne.' ') areach='S'    !  YES!!!
         filein='voacapx.dat'
         call get_command_argument(argCtr, fileout) !jw
         argCtr = argCtr + 1 !jw
         if(fileout(1:1).eq.' ') fileout='voacapb.out'
         nch_out=lcount(fileout,64)
         call unlink(run_directory(1:nch_run)//PATH_SEPARATOR//fileout(1:nch_out), istat)  
c         if(istat.ne.0 .and. iquiet.eq.0) then
c to insert an error mesage and exit here
c            call dos_error_message@(istat,message)
c         end if
      else
         call get_command_argument(argCtr, fileout) !jw
         argCtr = argCtr + 1 
         if(fileout(1:1).eq.' ') fileout='voacapx.out'
c         call lcase(fileout,20)
      end if
c      append=cmnam()       !  should we append to output file
      call get_command_argument(argCtr, append) !jw
      argCtr = argCtr + 1 !jw
      if(append.eq.'A') append='a'
c*****Check that the input file exists here...
c      inquire(file=filein,exist=doesit)
c      if(.NOT.doesit) goto 942 

      listing=areach
      if(listing.eq.'n') listing='N'
      if(listing.eq.'a') listing='A'
      if(filein (1:1).eq.' ' .or. fileout(1:1).eq.' ') then
         write(*,'('' Execute with: voacapw filein fileout'')')
         call exit(EC_EXEC_ERROR)
      end if
      iabort=0
c      call del_abt      !  delete the voacap.abt & voaarea.abt files
c******************************************************
C
C.....rewind INPUT LOGICAL UNITS BEFORE EXECUTION:
C
C                THIS IS THE USER-GENERATED INPUT FILE
40    continue
ccc      write(*,'(''after 40, area='',a)') areach
      if(areach.eq.'A' .or. areach.eq.'I') then  !  get real file name of output
         open(LU5,file=run_directory(1:nch_run)//PATH_SEPARATOR//filein, status='OLD', iostat=ios,err=943) 
         rewind(lu5)
         read(lu5,'(20x,a)') areafile
         close(lu5)
         nch=lcount(fileout,40)
         nch2=lcount(areafile,30)
         nch3=lcount(filein,20)
         areafile(nch2-2:nch2)='vg'//filein(nch3:nch3)
         fileout(nch+1:nch+nch2)=areafile(1:nch2)
      else if(listing.eq.'B') then           !  Initialize batch processing
         call read_asc('voacap',*999)  !read pt-to-pt common from VOACAPW.ASC
         open(38,file=run_directory(1:nch_run)//'/voacap.cir', status='old',err=999)
         rewind(38)
         read(38,'(a)',err=999) dum     !  skip 1st record
         if (iquiet.eq.0) then
            write(*,'('' Output is being written to: '',a,/)') fileout(1:nch_out)
         end if
         icircuit=0
         write(*,'('' starting batch '')')
         call batch(38,'VOACAP',filein,icircuit,*999)
      else if(listing.eq.'S') then           !  New SPECIAL batch
         nch_batch=lcount(file_batch,64)
         open(38,file=run_directory(1:nch_run)//PATH_SEPARATOR// file_batch(1:nch_batch),status='old',err=999)
         if (iquiet.eq.0) then
            write(*,'('' Output is being written to: '',a,/)') fileout(1:nch_out)
         end if
         icircuit=0
         call batch_S(38,'VOACAP',filein,icircuit,*999)
      end if
c***********************************************************
50    nch_in=lcount(filein,64)
      inquire(file=filein,exist=doesit)
      if(.NOT.doesit) goto 950 ! If the file doesn't exist, quit
      if(iquiet.eq.0) then
         if(filein(1:7).eq.'voacapw') write(*,51) 'WANTED  '
         if(filein(1:7).eq.'voacapu') write(*,51) 'UNWANTED'
51       format(' Calculating ',a,' signal')
      end if
      call antcalc(filein,listing)
c***********************************************************
      if(iquiet.eq.0) then
         if(listing.eq.'A') then
            write(*,'('' Area Input File    : '',a)') filein
            write(*,'('' Area Output File   : '',a)') fileout
            if(iarea_batch.ne.0) then
               alf_fileout=fileout
c               call window_update@(alf_fileout)
            end if
         else if(listing.eq.'I') then
            write(*,'('' Inverse Area Input File : '',a)') filein
         end if
      end if
c***********************************************************
      open(lu5,file=run_directory(1:nch_run)//PATH_SEPARATOR//filein, status='OLD', iostat=ios,err=945)
      rewind(lu5)
      ndistance=1
      if(fileout(1:11).eq.'VOACAPD.OUT' .or.
     +   fileout(1:11).eq.'voacapd.out') ndistance=51  !  plot vs distance
      ntime=0
      if(fileout(1:11).eq.'VOACAPT.OUT' .or.
     +   fileout(1:11).eq.'voacapt.out') ntime=1       !  plot vs time
      if(areach.eq.'B' .or. areach.eq.'S') then                  !  batch, use APPEND
         open(LU6,file=run_directory(1:nch_run)//PATH_SEPARATOR//fileout, access='APPEND')
         formfeed='\n\f'
      else if(fileout(1:2).eq.'..') then
         nchf=lcount(fileout,64)
         open(LU6,file=run_directory(1:nch_run-3)//fileout(4:nchf), iostat=ios,err=946)
         rewind(lu6)
         formfeed=' '
      else if(append.eq.'a') then
         open(LU6,file=run_directory(1:nch_run)//PATH_SEPARATOR//fileout, access='APPEND')
         formfeed='\n\f'
      else
         open(LU6,file=run_directory(1:nch_run)//'/'//fileout, iostat=ios,err=948)
         rewind(lu6)
         formfeed=' '
      end if
      open(LU15,file=run_directory(1:nch_run)//'/'//'lu15_voa.tmp', iostat=ios,err=960)
      open(LU35,file=run_directory(1:nch_run)//'/'//'lu35_voa.tmp', iostat=ios,err=962)
      rewind LU15
      rewind LU35
c***********************************************************
      if(ndistance.gt.1) then
         open(48,file=run_directory(1:nch_run)//'/voacapd.idx')
         rewind(48)
         call unlink(run_directory(1:nch_run)//'/voacapd.dst',istat)
         open(49,file=run_directory(1:nch_run)//'/voacapd.dst',
     +        access='direct',form='unformatted',recl=108)
      end if
      if(ntime.ne.0) then
         open(48,file=run_directory(1:nch_run)//'/voacapt.idx')
         rewind(48)
         call unlink(run_directory(1:nch_run)//'/voacap.dst',istat)
         open(49,file=run_directory(1:nch_run)//'/voacap.dst',
     +        access='direct',form='unformatted',recl=96)
      end if
c***********************************************************
c Version is read from voa_def.hdr
      open(21,file=run_directory(1:nch_run-3)//'database'//PATH_SEPARATOR//'version.'//
     +     COMPILER,status='old',iostat=ios,err=964)
      rewind(21)
      read(21,'(8x,a)') VERSN
      close(21)
ccc      write(*,'('' version='',a)') VERSN
c****************************************************************
      call set_magnetic_pole    !  get the location of the geomagnetic north pole
c****************************************************************
      iabort=1
      if(ndistance.gt.1 .or. ntime.ne.0) then
         call HFMUFS2(fileout,*950)           !  distance or time calculations
      else
         call HFMUFS(fileout,*950)            !  normal
      end if
      iabort=0          !  was not aborted
      if(i_S_to_I.ne.0) close(18)     !  close special S/I output file
      close(LU15)
      close(LU35)
      close(LU5)
      close(lu6)
      
c The following section has been modified from the original to 
c permit longer file extensions to be used (e.g. 'da12')
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
Cc****Start of error messages

c*****voaarea.cir batch area coverage file not found
920   write(*,'('' Error: Could not open file voaarea.cir for BATCH AREA: '',a)') trim(run_directory)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:920'')')
      call exit(EC_FILE_ERROR) ! Exit if we can't find the right file.

c*****This error is no longer called
940   write(*,'('' Error: voacapl not executed properly.'')')
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      call exit(EC_EXEC_ERROR)
      
c*****itshfbc directory not found
941   write(*,'('' Error: itshfbc directory not at specified path: '',a)') trim(run)
      write(*,'('' Run the command "makeitshbc" to create a copy of the'')')
      write(*,'('' itshfbc directory in your home directory.'')')
      write(*, '('' VOACAPW:941'')')
      call exit(EC_ITSHFBC_NOT_FOUND_ERROR) ! Exit if we can't find the right file.

c*****P2P prediction input file not found
942   write(*,'('' Error: The specified input file was not found: '',a)') run_directory(1:nch_run)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:942'')')
c      call exit(EC_INPUT_FILE_NOT_FOUND_ERROR) ! Exit if we can't find the right file.
      goto 950

c*****Area prediction input file not found
943   write(*,'('' Error: The specified input file was not found: '',a)') run_directory(1:nch_run)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:943'')')
      call exit(EC_INPUT_FILE_READ_ERROR) ! Exit if we can't find the right file.

c*****Missing or unreadable area data input file
944   write(*,'('' Error: The specified input file was not found: '',a)') '../areadata/'//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:'')')
      call exit(EC_FILE_ERROR) ! Exit if we can't find the right file.

c*****Missing or unreadable area data input file
945   write(*,'('' Error: The specified input file was not found: '',a)') run_directory(1:nch_run)//PATH_SEPARATOR//filein
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:945'')')
      call exit(EC_FILE_ERROR) ! Exit if we can't find the right file.

c*****Error opening output file
946   write(*,'('' Error opening output file : '',a)') run_directory(1:nch_run-3)//fileout(4:nchf)
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:946'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(EC_OUTPUT_FILE_OPEN_ERROR) ! Exit if we can't find the right file.

c*****Error opening output file
948   write(*,'('' Error opening output file : '',a)') run_directory(1:nch_run)//'/'//fileout
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:948'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(EC_OUTPUT_FILE_OPEN_ERROR) ! Exit if we can't find the right file.

c****Error opening the scratch file
960   write(*,'('' Error opening temporary scratch file : '',a)') run_directory(1:nch_run)//PATH_SEPARATOR//'lu15_voa.tmp'
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:960'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(EC_SCRATCH_FILE_OPEN_ERROR) ! Exit if we can't find the right file.

c****Error opening the scratch file
962   write(*,'('' Error opening file : '',a)') run_directory(1:nch_run)//PATH_SEPARATOR//'lu35_voa.tmp'
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:962'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(EC_FILE_ERROR) ! Exit if we can't find the right file.

c****Error opening the vesion file
964   write(*,'('' Error opening file : '',a)') run_directory(1:nch_run-3)//'database/version.'//COMPILER
      write(*,'('' Refer to the man page ("man voacapl") for help.'')')
      write(*, '('' VOACAPW:964'')')
      write(ci,'(I3)') ios
      ci = adjustl(ci)
      write(*,'('' Error code ('',a,'')'')') trim(ci)
      call exit(EC_FILE_ERROR) ! Exit if we can't find the right file.

c******************************************************************
950   close(LU15)
      close(LU35)
      if(iabort.ne.0) go to 999
      if(iarea_batch.eq.0) go to 999      !  not area coverage batch run
         read(61,'(a)',end=999) filein
         iarea_batch=iarea_batch+1
         if(iquiet.eq.0) write(*,39) iarea_batch,narea_batch,filein
         write(alf_iarea_batch,'(i4)') iarea_batch
c         call cpu_time(end_time)    !  use to calc time
c         elapsed=end_time-start_time
c         write(alf_elapsed_time,'(f8.1)') elapsed/60.
         fileout='voaareax'
         call areamap(areach,filein,fileout,area_meth)
         filein='voaareax.da1'
         fileout='../areadata/'
         if(areach.eq.'I') fileout='../area_inv/'
         go to 40      !  begin next area calculation
999   continue
      if(iarea_batch.ne.0) then      !  batch area finish
         idone=iarea_batch           !  number of files done
         iprocess_ctrl=0
c         call cpu_time(end_time)    !  jw
c         elapsed=end_time-start_time
         if(iquiet.eq.0) write(*,997) idone,elapsed/60.
997      format(50(1h*),/,
     +          i5,' BATCH Area calculations done.',f10.1,' minutes',/,
     +          50(1h*))
      end if
      if(listing.ne.'B' .and. iarea_batch.eq.0)then !don't destroy BATCH output
c         if(ierase.ne.0) call destroy_window(window_handle)
      else
         if(iabort.ne.0) write(*,998)
998                      format(/,' Batch processing has been aborted.')
      end if
c******************************************************************
c         close and delete temporary files
      close(LU15)
      close(LU35)
      call unlink(run_directory(1:nch_run)//PATH_SEPARATOR//'lu15_voa.tmp', istat) ! jw
      call unlink(run_directory(1:nch_run)//PATH_SEPARATOR//'lu35_voa.tmp', istat) ! jw
c      call underflow_count@(count_underflow)  !  see if any underflows occured
c      write(*,'(''underflow='',i8)') count_underflow
      call cpu_time(end_time)
c      print '("Time = ",f6.3," seconds.")',end_time-start_time
      END
c
c--------------------------------------------------------------
c###set_magnetic_pole.for
      SUBROUTINE set_magnetic_pole
c         This sets the location of the geomagnetic north pole
c         The default is (78.5N, 69.0W) = (78.5, -69.0)
      common /Cnorth_pole/ g_magnetic_lat,g_magnetic_lon  !  magnetic north pole
      common /crun_directory/ run_directory
         character run_directory*50
      data lu/21/

      character(len=1), parameter :: PATH_SEPARATOR ='/'

      nch_run=lcount(run_directory,50)
c***********************************************************
c          read the user values from ../run/north_pole.txt
      open(lu,file=run_directory(1:nch_run-3)//
     +   'run'//PATH_SEPARATOR//'north_pole.txt',status='old',err=200)
      rewind(lu)
      read(lu,*,err=150) glat,glon
      close(lu)
      if(glat.lt.60. .or. glat.gt.90.) go to 200       !  bad values of latitude 
      if(glon.lt.-180. .or. glon.gt.180.) go to 200    !  bad values of longitude 
      write(*,'('' ** Using user defined coordinates for North Pole **'')')
      go to 500     !  use these values
c***********************************************************
150   close(lu)
c          if user values do not exist,
c          read the default values from ../database/north_pole.txt
200   open(lu,file=run_directory(1:nch_run-3)//
     +   'database'//PATH_SEPARATOR//'north_pole.txt',status='old',err=300)
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
      return
      end
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
