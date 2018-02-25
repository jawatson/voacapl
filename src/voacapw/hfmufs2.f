c###hfmufs2.for
      SUBROUTINE HFMUFS2(fileout,*)   !  for vs DISTANCE & vs TIME plots
      use verbose_mod
      use version_mod
      character fileout*64,alf*4
      common /cdistance/ idistance,ndistance,ihr    !  plot vs distance
      common /ctime/ ntime                          !  plot vs time
      common /cnfreqs/ nfreqs
      common /crun_directory/ run_directory
         character run_directory*50
      common /cdaily/ idaily(12)
      common /Cday/ iday
C--------------------------------
C
C     THIS IS THE PROGRAM CONTROL SUBROUTINE
C     ITRUN CONTROLS PROGRAM TASK, ITOUT CONTROLS PROGRAM OUTPUT.
C
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
c      common /CVERSN/ VERSN
c         character VERSN*8
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      COMMON / SSP / SUN(2,12), MONTH
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
c------------------------------------------------------------------------
      iquiet=0
C.....START OF PROGRAM
C.....SET THE LOGICAL UNITS FOR SYSTEM INPUT AND OUTPUT FILES.
C     THE USER MUST INCLUDE AN "AUXIN" AND/OR AN "AUXOUT" CARD IF THE
C.....AUXILIARY INPUT AND/OR OUTPUT FILES ARE TO BE USED.
      LUI = LU5
      LUO = LU6
      nch_run=lcount(run_directory,50)
      call setvars     !  set variables that are undefined
      CALL LISTIN      !  create procedure file & check control card names
C.....TERMINATE PROGRAM IF ANY INVALID CONTROL CARD NAMES OCCUR
      IF(ITRUN.le.0) go to 600
C.....INPUT AND PROCESS CONTROL CARDS
C.....INPUT IS FORMATTED CARD IMAGES (USES DECODE)
      do 95 i=1,12
95    idaily(i)=0
100   CALL DECRED
C.....RETURN IF RUN OPTION IS .LE. 0
      IF(ITRUN.le.0) go to 600
c*****************************************************************
      MONTH=MONTHS(1)   !  only use 1st month/SSN
      SSN=SUNSP(1)
      call REDMAP(SSN,MONTH)
      iday=idaily(1)
      if(iday.ne.0) call redaily(ssn,month,iday)
      call GEOM
c*******************************************************
      do 10 nfreqs=1,11
      if(frel(nfreqs).eq.0.) go to 20
10    continue
20    nfreqs=nfreqs-1
      meth=method
      if(mspec.eq.121) meth=30
      write(*,21)meth,mspec,IMON(MONTH),nint(SSN),(frel(if),if=1,nfreqs)
21    format(' Method',2i4,1x,a3,i4,'ssn  Freqs=',11f5.1)
c*****************************************************************
      if(ndistance.ne.1) then       !  plots vs DISTANCE

         write(47,'(i5,'' distances'')') ndistance
         write(47,'(i2,11f7.3)') nfreqs,(frel(i),i=1,nfreqs)
         write(47,'(25i3)') nhours,(ihours(i),i=1,nhours)
         write(47,102)
 102     format('    id       gcdkm    Lat.     Lng.   Mode     MUF     FOT   ANGLE   DELAY   VHITE  ',
     +              'MUFday    LOSS     DBU    SDBW    NDBW     SNR   RPWRG     REL   MPROB    ',
     +              'SPRB   SIGLW   SIGUP   SNRLW   SNRUP   TGAIN   RGAIN   SNRxx      DBM')
         write(48,'(i5,'' distances'')') ndistance
         write(48,'(i2,11f7.3)') nfreqs,(frel(i),i=1,nfreqs)
         write(48,'(25i3)') nhours,(ihours(i),i=1,nhours)
         write(48,103)
 103     format(' 24 MODE  MUF   FOT   ANGLE DELAY VHITE MUFdayLOSS  ',
     +              'DBU   SDBW  NDBW  SNR   RPWRG REL   MPROB SPRB  ',
     +              'SIGLW SIGUP SNRLW SNRUP TGAIN RGAIN SNRxx DBM   ')
         if(iquiet.eq.0) then
            write(*,'('' Calculating Distance plot'')')
c            call soua@(' UT[')
            write(*,"(a)",advance='no') " UT["
         end if
      end if
      if(ntime.ne.0) then       !  plots vs TIME
         write(48,'(i2,11f7.3)') nfreqs,(frel(i),i=1,nfreqs)
         write(48,103)
         if(iquiet.eq.0) write(*,'('' Calculating Time plot'')')
      end if
      rlat_dist=rlat
      rlon_dist=rlong
      DO 405 ihr = 1,nhours         !  put hour as outside loop
      JT=ihours(ihr)
      if(ndistance.ne.1 .and. iquiet.eq.0) then
         write(alf,'(i3)') JT
c         call soua@(alf)
         write(*,"(a)",advance='no') alf
      end if
      do 400 idistance=1,ndistance
      if(idistance.eq.1) then
         rlatd=rlat_dist/d2r
         rlongd=rlon_dist/d2r
      else
         CALL distxy(idistance,ndistance,tlat,tlong,rlat_dist,rlon_dist,
     +               npsl,RLATD,RLONGD)
         tlongdx=tlongd
         if(tlongdx.lt.0.) tlongdx=tlongdx+360.    !  between 0 & 360
         if(abs(RLATD-TLATD).lt..02 .and.
     +      abs(RLONGD-TLONGDx).le..02) then  !  Tx & Rx cannot be same point
            RLONGD=TLONGDx+.02
            if(RLONGD.ge.360.) RLONGD=RLONGD-360.
         end if
         RLONG=RLONGD*D2R
         RLAT =RLATD *D2R
         if(abs(RLATD).gt.89.9) RLONG=0.    !  at poles, force long=0
      end if
      if(ndistance.eq.1) then
         if(ihr.eq.1) then
            CALL GEOM
            CALL SANG
            CALL SETOUT
         end if
      else
         CALL GEOM
         CALL SANG
         CALL SETOUT
      end if
c*****************************************************************
      IFREQ = FREL(14)       !  set control parameter for frequency complements
c      call yieldit               !  yield for Windows operations
      IUR=jt
      JTX = ihr
      CALL GEOTIM(JT)  !  convert UT to LMT, etc.
C.....WANT TO USE LONG TERM COEFFICIENTS
C.....TIME VARIATION OF MAPS
      CALL VIRTIM      !  UT VARIATION OF CRITICAL FREQUENCIES, F2M(3000)
      CALL TIMVAR      !  LMT dependency & E & F1 parameters
      CALL F2VAR       !  F2 parameters (F1 may be changed)
      CALL ESIND       !  Sporadic E parameters
      CALL IONSET
C.....MUF BY FULL IONOSPHERE, AND FOT CALCULATION (ITRUN = 4)
      CALL CURMUF
C.....FULL SYSTEMS PERFORMANCE MODEL (ITRUN = 7)
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
      ENDIF

      IF(meth.eq.30   .and. GCDKM.GE.GCDLNG) IPFG=200
      IF(GCDKM.GE.GCDLNG) IPFG=200
ccc      IF(GCDKM.GE.GCDLNG) mspec=0
      IF(meth.eq.21) IPFG=200      !  forced long path
      IF(meth.eq.22) IPFG=100      !  forced short path
ccc      write(* ,111) method,meth,mspec,npsl,ipfg,gcdkm,frel(12)
ccc111   format(' method,meth,mspec,npsl,gcdkm,muf=',5i5,f10.1,f10.3)
      CALL LUFFY(IPFG)
      CALL SETLUF
C
      CALL OUTLIN
  400 CONTINUE             !  end of DISTANCE loop
  405 CONTINUE             !  end of   HOUR   loop
      method=meth          !  reset METHOD
      GO TO 100
C.....END OF RUN
  600 CONTINUE
ccc      write(*,601) meth,method,mspec,gcdkm,gcdlng
ccc601   format('601=',3i5,2f10.2)
      WRITE(LUO,1504) VERSN
 1504 FORMAT(1H ,'*****END OF RUN*****',5X,'VOACAP ',a8)
      write(*,"(a)") " ]" ! Close the progress display and newline
      RETURN
      END
C--------------------------------
