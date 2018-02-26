c###sigdis.for
      SUBROUTINE SIGDIS
c***********************************************
c          Alex Shovkoplyas made some changes to:
c             curmuf
c             seltxr
c             settxr
c             sigdis
c          Normally if the version (..\database\version.w32)
c             is yy.mmddW then the original version is used.
c                yy.mmddA then Alex's version is used.
c***********************************************
      common /CVERSN/ VERSN
      character VERSN*8
      if(versn(8:8).eq.'A' .or. versn(8:8).eq.'a') then
         call sigdis_alex       !  use Alex's modified version
      else
         call sigdis_orig       !  use original version
      end if
      return
      end
C--------------------------------
      SUBROUTINE SIGDIS_orig
C--------------------------------
C
C  ADJUST SIGNAL DISTRIBUTION TABLES FOR THIS PATH AND SET ABSORPTION
C  LOSS PARAMETERS
C
      common /cversn/ versn
         character versn*8      !  look for "H" to ignore absorption fix
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),SIGPAT
     A(5), EPSPAT(5)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24),FOT
     A (24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMUF(4)
     B ,HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     C ,YMUF(4)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON/SIGD/DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE,HNU
     A ,HTLOSS,XNUZ,XVE
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      GLAV = 0.0
C.....SET NUMBER OF IONOSPHERES
      KFXX=KFX
ccc      KFXX=KM         !  temp change by Greg for receprocity try

      XKF  = KFXX
      XADJ = 0.0
      XSU  = 0.0
      XSL  = 0.0
      XFMP = 0.0
      XSUP = 0.0
      XSLP = 0.0
      AC = 0.0
      FEAV=0.0
ccc      write(luo,'(''in sigdis, kfx='',2i5)') kfx,km
      DO 115 K = 1,KFXX
C.....ABSORPTION INDEX
      ABIY(K) =  - 0.04 + EXP(-2.937 + 0.8445*FI(1,K) )
c****** Change made 6/2/99 by Greg Hand to fix absorption error
c       that was never fixed in VOACAP.
c******  per change to ICEPAC 3/18/93 by Frank Stewart to fix losses problem
c          this limits ABIY to a lower limit of .1 because that is the
c          lower limit of the curve.
c          IONCAP was wrong when it did not have the lower limit.
c          It was too optimistic with its prediction.
ccc      write(12,'(''abiy('',i1,'')='',f8.4)') k,abiy(k)
c          the "HAM" version does not do the absorption fix
c          to create the "HAM" version, edit the file:
c                  ..\database\version.w32
c             change the "W" to "H".
c          this was changed 2/8/2006 for Dean Straw
c          this was changed 4/18/2006 to "I" from "H". "I"=IONCAP
c          "I" or "a" is IONCAP. "a" is Alex's changes
      if(versn(8:8).eq.'I' .or. versn(8:8).eq.'a') then
      else
         ABIY(K)=MAX(0.1,ABIY(K))
      end if
C.....SYSTEM LOSS TABLE
      DUMMY1 = GLAT(K)
      DUMMY2 = CLCK(K)
      IDP=2
      IF(GCDKM.GT.2500.)IDP=5
      CALL SYSSY(DUMMY1,DUMMY2,IDP,ADJ,SU,SL,FMP,SUP,SLP)
ccc      write(luo,122) k,dummy1,dummy2,idp,adj,su,sl,fmp,sup,slp
ccc     +                ,abiy(k),fi(1,k)
ccc122   format('syssy=',i5,2e15.7,i5,8f10.4)
C.....MEDIAN
      XADJ = XADJ + ADJ
C.....UPPER DECILE
      XSU  = XSU  + SU
C.....LOWER DECILE
      XSL  = XSL  + SL
C.....PREDICTION ERROR, MEDIAN
      XFMP = XFMP + FMP
C.....UPPER DECILE
      XSUP = XSUP + SUP
C.....LOWER DECILE
      XSLP = XSLP + SLP
C.....E CRITICAL
      FEAV = FEAV + FI(1,K)
C.....ABSORBTION INDEX
      AC   = AC   + ABIY(K)
      ARTIC(K) = ADJ
C.....GEOMAGNETIC LATITUDE
      GLAV = GLAV + ABS(GLAT(K))
  115 CONTINUE
C.....BEGIN SECTION TO SAVE THE AVERAGE VALUES
      GLAV= GLAV/XKF
      AGLAT = GLAV
      ACAV= AC/XKF
      FEAV= FEAV/XKF
      ADJ = XADJ/XKF
ccc      write(luo,'(''adj='',4f11.4)') adj,xadj,xkf,acav
      SU  = XSU/XKF
      SL  = XSL/XKF
      ADS = XFMP/XKF
      SUS = XSUP/XKF
      SLS = XSLP/XKF
C.....END SECTION TO SAVE THE AVERAGE VALUES
      K = JMODE
C
C  PARAMETER FOR D-E ABSORTION LOSS.
C  ABIY( )  IS ABSORPTION INDEX. ACAV IS THE AVERAGE FOR THE PATH
C
C.....D - E REGION LOSS ADJUSTMENT FACTOR
      HTLOSS = 88.
      XVE    = XLIN( 90., HTRUE,FVERT,30, K )
      XVE    = XVE/FI(1,K)
      XNUZ = 63.07
      HNU  = 4.39
      IF(FEAV - 2.0) 125,125,120
C.....ADJUSTMENT TO CCIR 252 (HAYDON,LUCAS) LOSS EQUATION FOR E MODES
  120 AFE  = 1.359
      BFE  = 8.617
      GO TO 140
  125 IF(FEAV - 0. 5) 130,130,135
  130 AFE  = 0.0
      BFE  = 0.0
      GO TO 140
  135 AFE  = 1.359 *(FEAV - 0.5)/1.5
      BFE  = 8.617 *(FEAV - 0.5)/1.5
  140 CONTINUE
C
C  SET SIGNAL DISTRIBUTION TABLES. TABLE IS FOR FREQ = FTAB
C
C.....USE FOT, F2 LAYER
      GLAV = GLAV*R2D
      IF( GLAV - 40.)  145,145,150
  145 FTAB = YFOT(3)
      GO TO 165
  150 IF(GLAV  - 50.)  155,155,160
  155 FTAB = YFOT(3)
C.....INTERPOLATE IN BETWEEN
      FTAB = FTAB  - (GLAV - 40.)*(FTAB - 10.)/ 10.
      GO TO 165
C.....SET TO 10 MHZ (NEAR POLES)
  160 FTAB = 10.
  165 CONTINUE
      ESLSM = 0.0
      IF(YMUF(4).gt.0.) then
C   ES CONTRIBUTION TO TABLE( OBSCURTION LOSS).
C
C.....NOW REMOVE ES OBSCURATION AND F2 OVER-THE-MUF AT FTAB FROM TABLES
C.....AND WILL REPLACE FOR EACH MODE AND FREQUENCY AS NECESSARY
         DUMMY = YMUF(4)
         PES = PRBMUF(FTAB,DUMMY,DUMMY,4)
         PES =AMAX1(0.1,PES)
         PES =AMIN1(0.9,PES)
         ESLSM = -10.*ALOG10(1.-PES)
      end if
C  F2 OVER-THE-MUF CONTRIBUTION TO TABLE
C.....PROBABILITY CALCULATION
      DUMMY = YMUF(3)
      PF2 = PRBMUF(FTAB,DUMMY,DUMMY,3)
      PF2 = AMAX1(0.1, PF2)
ccc      PF2 = AMIN1(0.9, PF2)       !  this added 5/12/2005 GRH
      F2LSM = -10.*ALOG10(PF2)
      IF(IAND(INFO,2).GT.0)THEN
        WRITE(99,'(A,A,3F6.2)')' "SIGDIS" OTM(F2 NO SEC) LOSS E',
     1 'XCL FROM TAB 6 FTAB,YMUF,LOS=',FTAB,YMUF(3),F2LSM
      ENDIF
C RESIDUAL (AURORAL) LOSS ADJUSTMENT TO MEDIAN SIGNAL LEVEL.
      ASM = ADJ  - ESLSM - F2LSM
ccc      write(luo,123) asm,adj,eslsm,f2lsm,pf2
ccc123   format('ASM=',5f11.4)
      ASM = AMAX1(ASM, 0.0 )
C  UPPER DECILE
      PES = 0.0
      IF(YFOT(4).gt.0.) then
         DUMMY = YFOT(4)
         PES = PRBMUF(FTAB,DUMMY,DUMMY,4)
         PES = AMAX1( 0.1,PES)
         PES = AMIN1( 0.9,PES)
      end if
      DUMMY = YHPF(3)
      PF2 = PRBMUF(FTAB,DUMMY,DUMMY,3)
      PF2 = AMAX1(0.1, PF2)
ccc      PF2 = AMIN1(0.9, PF2)       !  this added 5/12/2005 GRH
C  UPPER DECILE SIGNAL LEVEL ADJUSTMENT TO MEDIAN
      DSU = 1.28* SL  -( +10.*ALOG10(1.- PES) + ESLSM)
     A                -( +10.*ALOG10(PF2) + F2LSM)
      DSU = AMAX1 ( DSU, 0.5)
C  LOWER DECILE.
      PES = 0.0
      IF(YHPF(4).gt.0.) then
         DUMMY = YHPF(4)
         PES = PRBMUF(FTAB,DUMMY,DUMMY,4)
         PES= AMAX1(0.1,PES)
         PES= AMIN1(0.9, PES)
      end if
      DUMMY = YFOT(3)
      PF2 = PRBMUF(FTAB,DUMMY,DUMMY,3)
      PF2 = AMAX1( 0.1,PF2)
ccc      PF2 = AMIN1(0.9, PF2)       !  this added 5/12/2005 GRH
C LOWER DECILE SIGNAL LEVEL ADJUSTMENT TO MEDIAN
      DSL = 1.28* SU - (-10.* ALOG10(1.-PES) - ESLSM)
     A               - (-10.* ALOG10(PF2) - F2LSM)
      DSL = AMAX1(1.,DSL)
      RETURN
      END
C--------------------------------
c###sigdis.for
      SUBROUTINE SIGDIS_alex
C--------------------------------
C
C  ADJUST SIGNAL DISTRIBUTION TABLES FOR THIS PATH AND SET ABSORPTION
C  LOSS PARAMETERS
C
      common /cversn/ versn
         character versn*8      !  look for "H" to ignore absorption fix
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),SIGPAT
     A(5), EPSPAT(5)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24),FOT
     A (24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMUF(4)
     B ,HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     C ,YMUF(4)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON/SIGD/DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE,HNU
     A ,HTLOSS,XNUZ,XVE
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
c>>>>>A.S.
      DIMENSION LX(3)
      DATA LX/1,3,5/
c<<<<<       
      GLAV = 0.0
C.....SET NUMBER OF IONOSPHERES
      KFXX=KFX
ccc      KFXX=KM         !  temp change by Greg for receprocity try

      XKF  = KFXX
      XADJ = 0.0
      XSU  = 0.0
      XSL  = 0.0
      XFMP = 0.0
      XSUP = 0.0
      XSLP = 0.0
      AC = 0.0
      FEAV=0.0
ccc      write(luo,'(''in sigdis, kfx='',2i5)') kfx,km
      DO 115 K = 1,KFXX
C.....ABSORPTION INDEX
      ABIY(K) =  - 0.04 + EXP(-2.937 + 0.8445*FI(1,K) )
c****** Change made 6/2/99 by Greg Hand to fix absorption error
c       that was never fixed in VOACAP.
c******  per change to ICEPAC 3/18/93 by Frank Stewart to fix losses problem
c          this limits ABIY to a lower limit of .1 because that is the
c          lower limit of the curve.
c          IONCAP was wrong when it did not have the lower limit.
c          It was too optimistic with its prediction.
ccc      write(12,'(''abiy('',i1,'')='',f8.4)') k,abiy(k)
c          the "HAM" version does not do the absorption fix
c          to create the "HAM" version, edit the file:
c                  ..\database\version.w32
c             change the "W" to "H".
c          this was changed 2/8/2006 for Dean Straw
c          this was changed 4/18/2006 to "I" from "H". "I"=IONCAP
c          "I" or "a" is IONCAP. "a" is Alex's changes
      if(versn(8:8).eq.'I' .or. versn(8:8).eq.'a') then
      else
         ABIY(K)=MAX(0.1,ABIY(K))
      end if
C.....SYSTEM LOSS TABLE
      DUMMY1 = GLAT(LX(K))
      DUMMY2 = CLCK(LX(K))
      IDP=2
      IF(GCDKM.GT.2500.)IDP=5
      CALL SYSSY(DUMMY1,DUMMY2,IDP,ADJ,SU,SL,FMP,SUP,SLP)
ccc      write(luo,122) k,dummy1,dummy2,idp,adj,su,sl,fmp,sup,slp
ccc     +                ,abiy(k),fi(1,k)
ccc122   format('syssy=',i5,2e15.7,i5,8f10.4)
C.....MEDIAN
      XADJ = XADJ + ADJ
C.....UPPER DECILE
      XSU  = XSU  + SU
C.....LOWER DECILE
      XSL  = XSL  + SL
C.....PREDICTION ERROR, MEDIAN
      XFMP = XFMP + FMP
C.....UPPER DECILE
      XSUP = XSUP + SUP
C.....LOWER DECILE
      XSLP = XSLP + SLP
C.....E CRITICAL
      FEAV = FEAV + FI(1,K)
C.....ABSORBTION INDEX
      AC   = AC   + ABIY(K)
      ARTIC(K) = ADJ
C.....GEOMAGNETIC LATITUDE
      GLAV = GLAV + ABS(GLAT(LX(K)))
  115 CONTINUE
C.....BEGIN SECTION TO SAVE THE AVERAGE VALUES
      GLAV= GLAV/XKF
      AGLAT = GLAV
      ACAV= AC/XKF
      FEAV= FEAV/XKF
      ADJ = XADJ/XKF
ccc      write(luo,'(''adj='',4f11.4)') adj,xadj,xkf,acav
      SU  = XSU/XKF
      SL  = XSL/XKF
      ADS = XFMP/XKF
      SUS = XSUP/XKF
      SLS = XSLP/XKF
C.....END SECTION TO SAVE THE AVERAGE VALUES
      K = JMODE
C
C  PARAMETER FOR D-E ABSORTION LOSS.
C  ABIY( )  IS ABSORPTION INDEX. ACAV IS THE AVERAGE FOR THE PATH
C
C.....D - E REGION LOSS ADJUSTMENT FACTOR
      HTLOSS = 88.
      XVE    = XLIN( 90., HTRUE,FVERT,30, K )
      XVE    = XVE/FI(1,K)
      XNUZ = 63.07
      HNU  = 4.39
      IF(FEAV - 2.0) 125,125,120
C.....ADJUSTMENT TO CCIR 252 (HAYDON,LUCAS) LOSS EQUATION FOR E MODES
  120 AFE  = 1.359
      BFE  = 8.617
      GO TO 140
  125 IF(FEAV - 0. 5) 130,130,135
  130 AFE  = 0.0
      BFE  = 0.0
      GO TO 140
  135 AFE  = 1.359 *(FEAV - 0.5)/1.5
      BFE  = 8.617 *(FEAV - 0.5)/1.5
  140 CONTINUE
C
C  SET SIGNAL DISTRIBUTION TABLES. TABLE IS FOR FREQ = FTAB
C
C.....USE FOT, F2 LAYER
      GLAV = GLAV*R2D
      IF( GLAV - 40.)  145,145,150
  145 FTAB = YFOT(3)
      GO TO 165
  150 IF(GLAV  - 50.)  155,155,160
  155 FTAB = YFOT(3)
C.....INTERPOLATE IN BETWEEN
      FTAB = FTAB  - (GLAV - 40.)*(FTAB - 10.)/ 10.
      GO TO 165
C.....SET TO 10 MHZ (NEAR POLES)
  160 FTAB = 10.
  165 CONTINUE
      ESLSM = 0.0
      IF(YMUF(4).gt.0.) then
C   ES CONTRIBUTION TO TABLE( OBSCURTION LOSS).
C
C.....NOW REMOVE ES OBSCURATION AND F2 OVER-THE-MUF AT FTAB FROM TABLES
C.....AND WILL REPLACE FOR EACH MODE AND FREQUENCY AS NECESSARY
         DUMMY = YMUF(4)
         PES = PRBMUF(FTAB,DUMMY,DUMMY,4)
         PES =AMAX1(0.1,PES)
         PES =AMIN1(0.9,PES)
         ESLSM = -10.*ALOG10(1.-PES)
      end if
C  F2 OVER-THE-MUF CONTRIBUTION TO TABLE
C.....PROBABILITY CALCULATION
      DUMMY = YMUF(3)
      PF2 = PRBMUF(FTAB,DUMMY,DUMMY,3)
      PF2 = AMAX1(0.1, PF2)
ccc      PF2 = AMIN1(0.9, PF2)       !  this added 5/12/2005 GRH
      F2LSM = -10.*ALOG10(PF2)
      IF(IAND(INFO,2).GT.0)THEN
        WRITE(99,'(A,A,3F6.2)')' "SIGDIS" OTM(F2 NO SEC) LOSS E',
     1 'XCL FROM TAB 6 FTAB,YMUF,LOS=',FTAB,YMUF(3),F2LSM
      ENDIF
C RESIDUAL (AURORAL) LOSS ADJUSTMENT TO MEDIAN SIGNAL LEVEL.
      ASM = ADJ  - ESLSM - F2LSM
ccc      write(luo,123) asm,adj,eslsm,f2lsm,pf2
ccc123   format('ASM=',5f11.4)
      ASM = AMAX1(ASM, 0.0 )
C  UPPER DECILE
      PES = 0.0
      IF(YFOT(4).gt.0.) then
         DUMMY = YFOT(4)
         PES = PRBMUF(FTAB,DUMMY,DUMMY,4)
         PES = AMAX1( 0.1,PES)
         PES = AMIN1( 0.9,PES)
      end if
      DUMMY = YHPF(3)
      PF2 = PRBMUF(FTAB,DUMMY,DUMMY,3)
      PF2 = AMAX1(0.1, PF2)
ccc      PF2 = AMIN1(0.9, PF2)       !  this added 5/12/2005 GRH
C  UPPER DECILE SIGNAL LEVEL ADJUSTMENT TO MEDIAN
      DSU = 1.28* SL  -( +10.*ALOG10(1.- PES) + ESLSM)
     A                -( +10.*ALOG10(PF2) + F2LSM)
      DSU = AMAX1 ( DSU, 0.5)
C  LOWER DECILE.
      PES = 0.0
      IF(YHPF(4).gt.0.) then
         DUMMY = YHPF(4)
         PES = PRBMUF(FTAB,DUMMY,DUMMY,4)
         PES= AMAX1(0.1,PES)
         PES= AMIN1(0.9, PES)
      end if
      DUMMY = YFOT(3)
      PF2 = PRBMUF(FTAB,DUMMY,DUMMY,3)
      PF2 = AMAX1( 0.1,PF2)
ccc      PF2 = AMIN1(0.9, PF2)       !  this added 5/12/2005 GRH
C LOWER DECILE SIGNAL LEVEL ADJUSTMENT TO MEDIAN
      DSL = 1.28* SU - (-10.* ALOG10(1.-PES) - ESLSM)
     A               - (-10.* ALOG10(PF2) - F2LSM)
      DSL = AMAX1(1.,DSL)
      RETURN
      END
C--------------------------------
