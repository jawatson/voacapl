c###hfmufs.for
      SUBROUTINE HFMUFS(fileout,*)
      character fileout*64,file_si*64
c jw      logical doesit*1
      logical*1 doesit
c jw      logical*4 fexists@
      integer*4 error_code4
      integer*2 error_code
      common /cQUIET/ iquiet
      common /crun_directory/ run_directory
         character run_directory*50
      common /cdaily/ idaily(12)
      common /Cday/ iday
      common /ccoeff/ coeff
         character coeff*4
      common /cCIRAF_TP/ nTP,idx_TP(911)
C--------------------------------
C
C     THIS IS THE PROGRAM CONTROL SUBROUTINE
C     ITRUN CONTROLS PROGRAM TASK, ITOUT CONTROLS PROGRAM OUTPUT.
C
      common /c_S_to_I/ i_S_to_I    !  = 1 = S/I calculation
      common /carea/ iarea             !  see if area coverage
      common /chours/ nhours,ihours(24)
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMIN,LTXRGM(2)
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      common /CVERSN/ VERSN
         character VERSN*8
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      COMMON / SSP / SUN(2,12), MONTH
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
C125PC      COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
C125PC      common/lpalmcmp/imddl(45,2,11,24,4,2),igdfx(45,2,11,24,4,2),
C125PC     +itlos(45,2,11,24,4,2),ihpfx(45,2,11,24,4,2),lNANG(2,11,24,4,2)
C125PC      integer*2 imddl,igdfx,itlos,ihpfx,lNANG
CPC      COMMON/MAPS/VMAP(12,6,50,90),PNT(2,3),ILT,ILG,NLAT,NLON,IMAP
c------------------------------------------------------------------------

C******************************************************************
      character(len=1), parameter :: PATH_SEPARATOR ='/'
C******************************************************************

C.....START OF PROGRAM
C.....SET THE LOGICAL UNITS FOR SYSTEM INPUT AND OUTPUT FILES.
C     THE USER MUST INCLUDE AN "AUXIN" AND/OR AN "AUXOUT" CARD IF THE
C.....AUXILIARY INPUT AND/OR OUTPUT FILES ARE TO BE USED.
      LUI = LU5
      LUO = LU6
      nch_run=lcount(run_directory,50)
      iarea=0
      call setvars     !  set variables that are undefined
C.....CREATE PROCEDURE FILE AND CHECK CONTROL CARD NAMES
      CALL LISTIN
C.....TERMINATE PROGRAM IF ANY INVALID CONTROL CARD NAMES OCCUR
      IF(ITRUN.le.0) go to 600
C.....INPUT AND PROCESS CONTROL CARDS
C.....INPUT IS FORMATTED CARD IMAGES (USES DECODE)
      ionce=0
      do 95 i=1,12
95    idaily(i)=0
100   CALL DECRED
C.....RETURN IF RUN OPTION IS .LE. 0
      IF(ITRUN.le.0) go to 600
      if(iarea.ne.0) then          !  area coverage
         if(nTP.eq.0) then         !  real area coverage
            call hfarea(*999)
         else
            call hfciraf(*999)     !  CIRAf zones for S/I analysis
         end if
         return
      end if
      if(i_S_to_I.ne.0) then        !  S/I calculation, open output file
         file_si=fileout
         nch=lcount(file_si,64)
         file_si(nch-3:nch)='.si '
         if(ionce.eq.0) then
            open(18,file=run_directory(1:nch_run)//PATH_SEPARATOR//file_si(1:nch))
            rewind(18)
            ionce=1
         end if
      end if
c*****************************************************************
c          mate MONTHs to SSNs per VOA request May 1993
c          required major changes to DECRED and following loop
      igraph=0                     !  do frel(1-11)
      if(frel(1).lt.0.) igraph=1   !  do frel=2 - 30 MHz
      do 550 imonth=1,12
c jw      call yieldit               !  yield for Windows operations
c          check to see if we should abort processing
ccc      inquire(file='voacap.abt',exist=doesit)
c jw      doesit=fexists@(
c jw     +    run_directory(1:nch_run)//'\voacap.abt',error_code4)
c jw      if(doesit) then      !  file exists, abort area calculations
c jw         call erase@(run_directory(1:nch_run)//'\voacap.abt',
c jw     +               error_code)   !  delete file first
c jw         return 1
c jw      end if
      MONTH=MONTHS(imonth)
      if(MONTH.eq.0) go to 100     !  no more months
      SSN=SUNSP(imonth)
ccc      if(igraph.eq.1) write(luo,9) imonth,month,nint(ssn)
ccc9     format(' COMMENT   GROUP ',i2,i5.2,i5,'ssn')
      call REDMAP(SSN,MONTH)
      iday=idaily(imonth)
      if(iday.ne.0) call redaily(ssn,month,iday)
      call GEOM
c*******************************************************
      nfr=igraph*2+1        !  (1 for non-graphs   3 for graphs)
      if(igraph.eq.1 .and. method.eq.26) nfr=1
      do 540 ifr=1,nfr
      if(igraph.eq.1) call runfreqs(ifr,frel)   !  freqs to run
      do 10 nf=1,11
      if(frel(nf).eq.0.) go to 20
10    continue
20    nf=nf-1
      meth=method
      if(mspec.eq.121) meth=30
      if(iquiet.eq.0) 
     +write(*,21) meth,IMON(MONTH),nint(SSN),(frel(if),if=1,nf)
21    format(' Method',i3,1x,a3,i4,'ssn  Freqs=',11f5.1)
      if(i_S_to_I.ne.0) then        !  S/I calculation, write to output file
         write(18,22) meth,MONTH,nint(SSN),nf,(frel(if),if=1,nf)
22       format('Method    ',4i5,11f7.3)
      end if
c*****************************************************************
C
C.....VALID RUN OPTION, BEGIN PROGRAM ANALYSIS
C.....REDUCE ANGLE SCAN FOR THIS DISTANCE
      CALL SANG
C.....PROCESS AND OUTPUT ANTENNA PATTERNS
      IF(ITRUN.eq.6) then
         CALL OUTANT
         GO TO 100
      end if
C.....SET OUTPUT ARRAYS
      CALL SETOUT
C.....SET CONTROL PARAMETER FOR FREQUENCY COMPLEMENTS
      IFREQ = FREL(14)
cD     IF(MSPEC.EQ.125)THEN
cc.......Power set to 1 Watt
cD       pwr=.001
cD       ISZ=ISZ+1
cD       IF(ISZ.GT.4)THEN
cD         ISP=ISP+1
cD         IF(ISP.GT.2)THEN
cD           do 128 ISP=1,2
cD           do 128 ISZ=1,4
cD           do 128 IUR=1,24
cD           do 128 IFQN=1,11
cc`````````````````````````````````````````````````````````````````````
cD           do 129 IEND=1,2
cD129        lNANG(IEND,IFQN,IUR,ISZ,ISP)=0
cc-------------------------------------------------------------------
cD128        NMS(IFQN,IUR,ISZ,ISP)=-1
cD           ISP=1
cD         ENDIF
cD         ISZ=1
cD       ENDIF
cD     ENDIF
C.....BEGIN HOUR LOOP
      DO 400 ihr = 1,nhours
c jw      call yieldit               !  yield for Windows operations
      JT=ihours(ihr)
      IUR=jt
      JTX = ihr
C.....CONVERT UT TO LMT, ETC.
      CALL GEOTIM(JT)
C.....WANT TO USE LONG TERM COEFFICIENTS
      IF(KRUN.gt.2) go to 140
C.....TIME VARIATION OF MAPS
C.....UT VARIATION OF CRITICAL FREQUENCIES, F2M(3000)
      CALL VIRTIM
      IF(ITRUN.le.0) go to 600
      IF(KRUN.gt.1) go to 135
C.....LMT DEPENDENCY AND E AND F1 PARAMETERS
      CALL TIMVAR
C.....F2 PARAMETERS (F1 MAY BE CHANGED)
      CALL F2VAR
C.....SPORADIC E PARAMETERS
      IF(KRUN.ge.1) go to 140
  135 CALL ESIND
C.....USE ITRUN TO CONTROL PROGRAM EXECUTION
  140 IF(ITRUN - 2) 145, 150, 155
C.....IONOSPHERIC PARAMETERS ONLY (ITRUN = 1)
  145 CALL OUTPAR
      GO TO 300
C.....IONOGRAMS ONLY (ITRUN = 2)
  150 CALL IONSET
      CALL OUTION
      GO TO 300
C.....MUF BY CLASSICAL NOMOGRAMS ONLY (ITRUN = 3)
  155 CALL IONSET
      IF(ITRUN.eq.3) then
         CALL NOMMUF
         GO TO 300
      end if
C.....MUF BY FULL IONOSPHERE, AND FOT CALCULATION (ITRUN = 4)
      CALL CURMUF
      IF(ITRUN.eq.4) go to 300
C.....MUF AS A FUNCTION OF K LINES (ITRUN = 5) (NOT YET DEVELOPED)
      IF(ITRUN.eq.5) go to 410
C.....FULL SYSTEMS PERFORMANCE MODEL (ITRUN = 7)
      IF(ITRUN.ne.7) go to 250
      IF(IFREQ.le.0) then
C............SET FREQUENCY COMPLEMENT
         CALL FRQCOM(FREL,IFREQ)
      else
         FREL(12) = ALLMUF(IT)
      end if
      IF(METHOD.EQ.22.OR.METHOD.EQ.25.OR.MSPEC.EQ.125)THEN
C.......FORCE SHORT PATH MODEL
        IPFG=100
      ELSE IF(METHOD.EQ.21) THEN
        IF(MSPEC.EQ.121)THEN
C.........SET FOR SHORT PATH AND USE LONGPATH/SHORTPATH SMOOTHING
          IPFG=100
        ELSE
C.........FORCE LONG PATH MODEL
          IPFG=200
        ENDIF
      ELSE
C.......FORCE LONG PATH MODEL IF PATH LENGTH .GT. SPECIFIED LIMIT
        IPFG=100
        IF(MSPEC.ne.121 .and. GCDKM.GE.GCDLNG) IPFG=200
        IF(meth.eq.30   .and. GCDKM.GE.GCDLNG) IPFG=200
      ENDIF
ccc      if(mspec.eq.121) 
ccc     +    write(12 ,111) method,meth,npsl,mspec,gcdkm,frel(12)
ccc111   format(' method,npsl,mspec,gcdkm,muf=',4i5,f10.1,f10.3)
      CALL LUFFY(IPFG)
      CALL SETLUF
      GO TO 300
C.....CALCULATE LUF (ITRUN = 8)
  250 IF(ITRUN.eq.8) then
         IPFG=300
         IF(GCDKM.GE.GCDLNG)IPFG=400
         CALL LUFFY(IPFG)
      end if
  260 CONTINUE
C.....USE ITOUT TO CONTROL PROGRAM OUTPUT
C
  300 if(MSPEC.eq.125)go to 400
      IF(ITOUT.eq.7) then        !  output printed lines dependent on METHOD
         CALL OUTLIN
      else IF(ITOUT.eq.8) then   !  output reliabilities
         CALL OUTTAB
      else IF(ITOUT.eq.10) then  !  output FOT-MUF-HPF for full ionosphere
         CALL OUTLAY
      end if
  400 CONTINUE
C.....END OF THE HOUR LOOP
  410 IF(ITOUT.eq.3) then
         CALL OUTMUF
         GO TO 450
      end if
C.....OUTPUT DIURNAL PRINTER PLOTS
      IF(ITOUT.eq.4) then
         CALL OUTGPH
         GO TO 450
      end if
C.....OUTPUT FOR MUF AS A FUNCTION OF K.
ccc      IF(ITRUN.eq.5) CALL OUTKMF
C.....OUTPUT ADDITIONAL DIURNAL GRAPHS IF REQUESTED BY "OUTGRAPH" CARD
  450 IF(ISOUT.le.0) go to 500
C.....IGNORE REQUEST IF CURRENT METHOD DID NOT COMPUTE MUFS OR LUFS
      IF(ITRUN.le.2) go to 500
      IF(ITRUN.le.4) go to 459
      IF(ITRUN.le.6) go to 500
      IF(ITRUN.gt.8) go to 500
C.....SAVE THE CURRENT METHOD, RUN AND OUTPUT OPTIONS
  459 ITEMP = METHOD
      JTEMP = ITRUN
      KTEMP = ITOUT
      DO 495 I = 1,12
      METHOD = KTOUT(I)
C.....IGNORE REQUEST IF IT IS FOR OUTPUT ALREADY SUPPLIED
      IF(METHOD.eq.ITEMP) go to 495
C.....IGNORE REQUEST IF AN INVALID METHOD IS SPECIFIED
      METNEG = 0
      IF(METHOD) 465, 495, 469
C.....INTERCHANGE OUTPUT FILES IF METHOD IS NEGATIVE ON "OUTGRAPH" CARD
  465 METNEG = 1
      METHOD = - METHOD
      IF(LUO.eq.LU6) then
         LUO = LU16
      else
         LUO = LU6
      end if
  469 IF(METHOD.ge.30) go to 490
      ITOUT = JTOUT(METHOD)
      ITRUN = JTRUN(METHOD)
C.....IGNORE THE REQUEST IF OUTPUT OPTION OF METHOD SPECIFIED ISN"T 3or4
      IF(ITOUT - 3) 475, 480, 475
  475 IF(ITOUT - 4) 490, 485, 490
C.....IGNORE REQUEST IF FOR LUF AND LUF HASN"T BEEN COMPUTED OR SET
C.....USING FREQUENCY COMPLEMENT AND RELIABLITIES
  480 IF(ITRUN - 8) 484, 481, 484
  481 IF(JTEMP - 7) 490, 484, 482
  482 IF(JTEMP - 8) 490, 484, 490
  484 CALL OUTMUF
      GO TO 490
  485 IF(ITRUN - 8) 488, 486, 488
  486 IF(JTEMP - 7) 490, 488, 487
  487 IF(JTEMP - 8) 490, 488, 490
  488 CALL OUTGPH
C.....RESTORE ORIGINAL OUTPUT FILES
  490 IF(METNEG.le.0) go to 495
  491 IF(LUO.eq.LU6) then
         LUO = LU16
      else
         LUO = LU6
      end if
  495 CONTINUE
C.....RESTORE THE ORIGINAL METHOD, RUN AND OUTPUT OPTIONS
      METHOD = ITEMP
      ITRUN = JTEMP
      ITOUT = KTEMP
C.....END OF EXECUTION (RETURN TO READ NEXT INPUT CARD)
  500 CONTINUE
  540 CONTINUE
  550 CONTINUE
      GO TO 100
C.....END OF RUN
  600 CONTINUE
      WRITE(LUO,1504) VERSN
 1504 FORMAT(1H ,'*****END OF RUN*****',5X,'VOACAP ',a8)
      RETURN
999   return 1     !  area coverage  or batch abort
      END
C--------------------------------
