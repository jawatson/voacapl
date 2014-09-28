c# hfarea.f
      SUBROUTINE hfciraf(*)    !  output routine for CIRAF test points
c      common /Ccancel_batch/ icancel_batch
c      common /cQUIET/ iquiet
      use verbose_mod
      common /cCIRAF_TP/ nTP,idx_TP(911)
      common /Cciraf/ nciraf_tp,ciraf(911),ciraf_lat(911),ciraf_lon(911)
         character ciraf*4
C--------------------------------
C
C     THIS IS THE PROGRAM CONTROL SUBROUTINE
C     ITRUN CONTROLS PROGRAM TASK, ITOUT CONTROLS PROGRAM OUTPUT.
C
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
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      COMMON /RTANT /XETA, XSIG, XEPS, XND, XNL, XNH, TEX (4), ITANT,
     1 IRANT, RETA, RSIG, REPS, RND, RNL, RNH, REX(4), TEFF, REFF,
     2 KASANT(2), ITSANT, JTSANT
      COMMON / SSP / SUN(2,12), MONTH
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
*D     COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
*D     common/lpalmcmp/imddl(45,2,11,24,4,2),igdfx(45,2,11,24,4,2),
*D    +itlos(45,2,11,24,4,2),ihpfx(45,2,11,24,4,2),lNANG(2,11,24,4,2)
*D     integer*2 imddl,igdfx,itlos,ihpfx,lNANG
      COMMON /RGRID/ IPROJ,PLAT,PLON,XMIN,XMAX,YYMIN,YMAX,NX,NY
      common /crun_directory/ run_directory
         character run_directory*50
      common /cdaily/ idaily(12)
      common /Cday/ iday
      character alf*20
      logical*1 doesit
      integer*4 error_code4
      integer error_code
c------------------------------------------------------------------------
C.....START OF PROGRAM
C.....SET THE LOGICAL UNITS FOR SYSTEM INPUT AND OUTPUT FILES.
C     THE USER MUST INCLUDE AN "AUXIN" AND/OR AN "AUXOUT" CARD IF THE
C.....AUXILIARY INPUT AND/OR OUTPUT FILES ARE TO BE USED.
      LUI = LU5
      LUO = LU6
c***********************************************************************
      nch_run=lcount(run_directory,50)
      MONTH=MONTHS(1)
      SSN=SUNSP(1)
      call REDMAP(SSN,MONTH)
      iday=idaily(1)            !  day of the month (if used)
      if(iday.ne.0) call redaily(ssn,month,iday)
      nf=1
      do 5 nf=1,11
      if(frel(nf).eq.0.) go to 6
5     continue
6     nf=nf-1
      call outciraf(-nTP,0)                !  output headers
      meth=METHOD
      if(MSPEC.eq.121) meth=30             !  smoothing method
      if(nf.eq.1) then
         if(iquiet.eq.0) 
     +   write(*,21) meth,IMON(MONTH),IHRO,nint(SSN),frel(1)
21       format(20x,' Method',i3,1x,a3,i3.2,'ut',i4,'ssn  Freqs=',f6.2)
      else
         if(iquiet.eq.0) 
     +   write(*,22) meth,IMON(MONTH),IHRO,nint(SSN),nf
22       format(20x,' Method',i3,1x,a3,i3.2,'ut',i4,'ssn  max of',
     +          i3,' Freqs')
      end if
c*****************************************************************
      DO 900 itp=1,nTP            !  loop through CIRAF test points
c          check to see if we should abort processing
c      inquire(file="/voaarea.abt",exist=doesit)
c      if(doesit) then      !  file exists, abort area calculations
c         call unlink(run_directory(1:nch_run)//'/voaarea.abt',
c     +                  error_code)   !  delete file first
c         return 1
c      end if
c      if(icancel_batch.ne.0) return     !  other cancel button picked
c      call yieldit    !  yield for Windows
      jtp=idx_TP(itp)                      !  get CIRAF test point number
      rlongd=ciraf_lon(jtp)
      rlatd =ciraf_lat(jtp)
      if(abs(RLATD-TLATD).lt..05 .and. abs(RLONGD-TLONGD).le..05) then
         RLONGD=TLONGD+.05
      end if
      do 10 i=1,24
10    allmuf(i)=-1.
ccc      NPSL=0            !  shortest path  (get NPSL from CIRCUIT card)
ccc      NPSL=1            !  thru antipode
C==================================================================
      CALL GEOM
      CALL SANG         !  reduce angle scan for this distance
C.....SET OUTPUT ARRAYS
      CALL SETOUT
C.....SET CONTROL PARAMETER FOR FREQUENCY COMPLEMENTS
      IFREQ = 1
C.....BEGIN HOUR LOOP
      JT = IHRO
      IUR=jt
      JTX = JT
      CALL GEOTIM(JT)   !  convert UT to LMT, etc
C.....WANT TO USE LONG TERM COEFFICIENTS
C.....TIME VARIATION OF MAPS
      CALL VIRTIM       !  UT variation of critical frequencies, F2M(3000)
      CALL TIMVAR       !  LMT dependency and E and F1 parameters
      CALL F2VAR        !  F2 parameters (F1 may be changed)
      CALL ESIND        !  Sporatic E parameters
      CALL IONSET
      CALL CURMUF       !  MUF by full ionosphere, and FOT calculation
      FREL(12) = ALLMUF(IT)
      IF(METHOD.EQ.22.OR.METHOD.EQ.25.OR.MSPEC.EQ.125)THEN
C.......FORCE SHORT PATH MODEL
        IPFG=100
      ELSE IF(METHOD.EQ.21)THEN
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
        IF(MSPEC.ne.121 .and. GCDKM.GE.GCDLNG)IPFG=200
      ENDIF
ccc      write(luo,111) ix,iy,method,npsl,ipfg,gcdkm,frel(12)
ccc111   format(5i5,f10.1,f10.3)
      CALL LUFFY(IPFG)
      CALL SETLUF
      call outciraf(itp,jtp)
C==================================================================
900   CONTINUE
C******************************************************************
C.....END OF RUN
      RETURN
      END
c------------------------------------------------------------
