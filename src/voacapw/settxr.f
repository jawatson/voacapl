c###settxr.for
      SUBROUTINE SETTXR
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
c      common /CVERSN/ VERSN
c      character VERSN*8
      use version_mod
      if(versn(8:8).eq.'A' .or. versn(8:8).eq.'a') then
         call settxr_alex       !  use Alex's modified version
      else
         call settxr_orig       !  use original version
      end if
      return
      end
C--------------------------------
      SUBROUTINE SETTXR_orig
C--------------------------------
C------------ SUBROUTINES SETRCR AND SETTMT COMBINED  1/10/91 FJR
C
C     THIS SUBROUTINE CALCULATES THE LOSSES, ETC. FOR ALL
C     MODES AT THE TRANSMITTER/RECEIVER END
C
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
      COMMON/ES/FS(3,5),HS(5)
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),SIGPAT
     A (5),EPSPAT(5)
      COMMON/LOSX/ANDVX(45,3),ADVX(45,3),AOFX(45,3),ARFX(45,3),GRLOSX(45
     A ,3),TGAINX(45,3),TLSKM(45,3),EFFlp(45),IAFTXR(3)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24)
     A ,ALLMUF(24),FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4)
     B ,SIGU(4),DELMUF(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4)
     C ,YFOT(4),YHPF(4),YMUF(4)
      COMMON/SIGD/DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE,HNU
     A ,HTLOSS,XNUZ,XVE
      COMMON/RAYS/ANG(40),IFOB(40,30,5),NANG
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON / allMODE /ABPS(20),CREL(20),FLDST(20),HN(20),HP(20),
     1PROB(20),RELY(20),RGAIN(20),SIGPOW(20),SN(20),
     2SPRO(20),TGAIN(20),TIMED(20),TLOSS(20),B(20),FSLOS(20),
     3grlos(20),adv(20),obf(20),
     CNMODE(20),TLLOW(20),TLHGH(20),EFF(20),NREL,NMMOD
C125PC      common/lpalmcmp/imddl(45,2,11,24,4,2),igdfx(45,2,11,24,4,2),
C125PC     +itlos(45,2,11,24,4,2),ihpfx(45,2,11,24,4,2),lNANG(2,11,24,4,2)
C125PC      integer*2 imddl,igdfx,itlos,ihpfx,lNANG
      COMMON/INFORM/INFO,IHSHR,IHLNG
C125PC      COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
c----------------------------------------------------------------
C.....FOR MAX. NUMBER OF HOPS
      DEND = AMIN1(4000.,GCDKM)
ccc      write(12,'(''amind='',f10.5)') amind
      DO 1000 JJ=1,2
      ILN=0
      K = ITXRCP(JJ)
      DO 10 IA=1,45
      GML(IA,K)=-999.
   10 FHP(IA,K)=999.
      DO 80 IA = 1,45
C.....DOES THE MODE EXIST (QUESTION MARK)
      IF(HPFLX(IA,K) - 70. ) 90,75,75
C.....NUMBER OF HOPS
   75 XHPM = DEND/GDFLX(IA,K)
C.....MUST HAVE AT LEAST ONE FOR SHORT PATHS
      IF( XHPM - 0.9) 80, 76, 76
   76 IF(DELFX(IA,K) - AMIND) 80, 95, 95
   80 CONTINUE
   90 CONTINUE
C  PENETRATED LAYER, TRY OVER THE MUF MODE.
C.....SO IF NONE FAILS, INCLUDE OVER-THE-MUF
      IM = MODMUF
      DELFX(1,K) = DELMUF(IM)
      HPFLX(1,K) = HPMUF (IM)
      HTFLX(1,K) = HTMUF (IM)
      FVFLX(1,K) = FVMUF (IM)
      AFFLX(1,K) = AFMUF (IM)
      XHOP = NHOPMF(IM)
      GDFLX(1,K) = GCDKM/XHOP
      IMODE(1,K) = MODMUF
   95 CONTINUE
      BC = (FREQ+ GYZ(K))**1.98
      AC = 677.2*ABIY(K)
      DO 250 IA = 1,45
C.....PRESET ARRAYS
      TGAINX(IA,K) = -10.
      ANDVX(IA,K) = 1000.
      ADVX (IA,K) = 1000.
      GRLOSX(IA,K) = 0.0
C.....MODE GOOD (QUESTION MARK)  ONE IS.
ccc      write(12,14) ia,k,hpflx(ia,k),delfx(ia,k)
ccc14    format('ia,k,hpflx,delfx=',2i5,2f10.4)
      IF(HPFLX(IA,K).lt.70.) go to 250
C.....CHECK FOR MINIMUM
      IF(DELFX(IA,K).lt.AMIND) go to 250
      DEL = DELFX(IA,K) * D2R      !  radiation angle
      CDEL = COS(DEL)
C.....E MODE (QUESTION MARK)
      IF(FI(1,K).lt.FVFLX(IA,K) ) go to 125
C  D-E  LAYER MODES.
C.....VERY LOW REFLECTIONS (QUESTION MARK)
      IF(HTLOSS.le.HTFLX(IA,K)) then
         XNSQ = 10.2
      else
         HNUX = 61.+ 3.*(HTFLX(IA,K)- 70.)/18.
C.....COLLISION FREQUENCY
         XNSQ = XNUZ * EXP(-2.*(HNUX -60.)/HNU)
      end if
      HEFF = AMIN1(100.,HTFLX(IA,K))
      SINP = RZ* CDEL/(RZ+ HEFF)
      SECP = 1./SQRT(1.- SINP*SINP)
C.....ABSORPTION LOSS
      ANDVX(IA,K)= SECP *AC /(BC +XNSQ)
      XV = AMAX1(FVFLX(IA,K)/FI(1,K) , XVE)
C.....CORRECTION FOR E LAYER
      ADX= AFE + BFE* ALOG(XV)
      SINP = RZ*CDEL/(RZ +HPFLX(IA,K))
      SECP = 1./SQRT(1.-SINP*SINP)
C.....DEVIATIVE LOSS + CORRECTION
      ADVX(IA,K) = SECP*AFFLX(IA,K)*( ( FVFLX(IA,K)+GYZ(K))**1.98 +XNSQ)
     A            /(BC+ XNSQ)   +ADX
C.....ES OBSCURATION, NONE
      AOFX(IA,K) = 0.0
      GO TO 160
C  F LAYER MODES
  125 CONTINUE
      XNSQ = 10.2
      SINP = RZ*CDEL/(RZ +100.)
      SECP = 1./SQRT(1. -SINP*SINP)
C.....ABSORPTION LOSS
      ANDVX(IA,K) = SECP*AC/(BC+ XNSQ)
      SINP = RZ* CDEL/(RZ +HPFLX(IA,K))
      SECP = 1./SQRT(1. -SINP*SINP)
C.....DEVIATIVE LOSS
      ADVX(IA,K) = SECP*AFFLX(IA,K)*((FVFLX(IA,K)+GYZ(K))**1.98 +XNSQ)
     A            /(BC+ XNSQ)
C  ES OBSCURTION LOSS
      AOFX(IA,K) =0.0
C.....SPORADIC E EXISTS (QUESTION MARK)
      IF(FS(2,K).le.0.) go to 160
      FSDEAD =IFOB(1,3,K)
      FSDEAD =FSDEAD/1000.
      FMHZ = AMAX1(FREQ,FSDEAD)
      SINS = RZ*CDEL/(RZ + HS(K) )
      SECS = 1./SQRT(1.-SINS*SINS)
C.....MUF THIS DISTANCE
      ESD  = FS(2,K)*SECS
      DUMMY = YMUF(4)
      P = PRBMUF(FMHZ,ESD,DUMMY,4)
      P = AMAX1(.1,P)
      P = AMIN1(.9,P)
C.....OBSCURATION
      AOFX(IA,K) = -10.*ALOG10(1.-P)
  160 CONTINUE
C GROUND LOSS
      Y = 0.0
      DO 170 IG = 1,KM
      IGX = -IG
      CALL GAIN(IGX,DEL,FREQ,YG,GEFF)
  170 Y = Y + YG
      XKM = KM
C.....AVERAGE GROUND LOSS
      GRLOSX(IA,K) = Y / XKM
C ANTENNA GAINS
C125PC      IF(MSPEC.EQ.125)THEN
C125PC        TGAINX(IA,K) =0.
C125PC      ELSE
        CALL GAIN(JJ,DEL,FREQ,Y,TEFF)
        TGAINX(IA,K) =Y
        if(JJ.eq.2)EFFlp(ia)=TEFF
C125PC      ENDIF
      SPHET = RZ * CDEL / (RZ + HTFLX(IA,K))
      CPHET = 1.-SPHET*SPHET
      CPHET = AMAX1(0.00000001,CPHET)
      CPHET = SQRT(CPHET)
C-----?????????????????
      IS = IMODE(IA,K)
      IS = MODMUF
C-----?????????????????
      DUMMY = YMUF(IS)
      PROS = PRBMUF(FREQ,DUMMY,DUMMY,IS)
C.....OVER-THE-MUF, MEDIAN
      XLS = -10.*ALOG10(PROS)/CPHET
      IF(IAND(INFO,2).GT.0)THEN
        XLSWO=XLS*CPHET
        WRITE(99,'(2(A,I2),A,2F7.2,A,2F7.2)')' "SETTXR" LAY=',IS,
     1'  IA=',IA,'  FRQ,YMUF=',FREQ,DUMMY,'   MD OTMs,OTM=',XLS,
     2  XLSWO
      ENDIF
C ADD ADJUSTMENT TO ABSORPTION LOSS
      ANDVX(IA,K) = ANDVX(IA,K) + XLS
      PROB(JJ) = PROS
      ILN=ILN+1
C125PC      IF(MSPEC.EQ.125)THEN
C125PC      itlos(ILN,JJ,IFQN,IUR,ISZ,ISP)=(andvx(ia,k)+advx(ia,k)+.05)*10.
C125PC      imod=IMODE(ia,k)
C125PC      imddl(ILN,JJ,IFQN,IUR,ISZ,ISP)=imod*10000+(delfx(ia,k)+.05)*10.
C125PC      ihpfx(ILN,JJ,IFQN,IUR,ISZ,ISP)=(hpflx(ia,k)+.05)*10.
C125PC      igdfx(ILN,JJ,IFQN,IUR,ISZ,ISP)=(GDFLX(IA,K)+.5)
C125PC      ENDIF
  250 CONTINUE
C125PC      IF(MSPEC.EQ.125)LNANG(JJ,IFQN,IUR,ISZ,ISP)=ILN
 1000 CONTINUE
      RETURN
      END
C--------------------------------
c###settxr.for
      SUBROUTINE SETTXR_alex
C--------------------------------
C------------ SUBROUTINES SETRCR AND SETTMT COMBINED  1/10/91 FJR
C
C     THIS SUBROUTINE CALCULATES THE LOSSES, ETC. FOR ALL
C     MODES AT THE TRANSMITTER/RECEIVER END
C
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
      COMMON/ES/FS(3,5),HS(5)
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),SIGPAT
     A (5),EPSPAT(5)
      COMMON/LOSX/ANDVX(45,3),ADVX(45,3),AOFX(45,3),ARFX(45,3),GRLOSX(45
     A ,3),TGAINX(45,3),TLSKM(45,3),EFFlp(45),IAFTXR(3)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24)
     A ,ALLMUF(24),FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4)
     B ,SIGU(4),DELMUF(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4)
     C ,YFOT(4),YHPF(4),YMUF(4)
      COMMON/SIGD/DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE,HNU
     A ,HTLOSS,XNUZ,XVE
      COMMON/RAYS/ANG(40),IFOB(40,30,5),NANG
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON / allMODE /ABPS(20),CREL(20),FLDST(20),HN(20),HP(20),
     1PROB(20),RELY(20),RGAIN(20),SIGPOW(20),SN(20),
     2SPRO(20),TGAIN(20),TIMED(20),TLOSS(20),B(20),FSLOS(20),
     3grlos(20),adv(20),obf(20),
     CNMODE(20),TLLOW(20),TLHGH(20),EFF(20),NREL,NMMOD
C125PC      common/lpalmcmp/imddl(45,2,11,24,4,2),igdfx(45,2,11,24,4,2),
C125PC     +itlos(45,2,11,24,4,2),ihpfx(45,2,11,24,4,2),lNANG(2,11,24,4,2)
C125PC      integer*2 imddl,igdfx,itlos,ihpfx,lNANG
      COMMON/INFORM/INFO,IHSHR,IHLNG
C125PC      COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
c----------------------------------------------------------------
c>>>>>A.S.
      DIMENSION LX(3)
      DATA LX/1,3,5/
      LOGICAL FOUND
c<<<<<       
C.....FOR MAX. NUMBER OF HOPS
      DEND = AMIN1(4000.,GCDKM)
ccc      write(12,'(''amind='',f10.5)') amind
      DO 1000 JJ=1,2
      ILN=0
      K = ITXRCP(JJ)
      DO 10 IA=1,45
      GML(IA,K)=-999.
   10 FHP(IA,K)=999.
   
c>>>>>A.S.
c     check if any good modes exist
c     mark bad modes with HPFLX = 0
      FOUND = .FALSE.
      DO 80 IA = 1,45
      IF(IA .GT. IAFTXR(K)) GOTO 75                  !end of HPFLX reached
      IF(HPFLX(IA,K) .LT. 70) GOTO 75                !end of HPFLX reached
      IF(DELFX(IA,K) .LT. AMIND) GOTO 75             !below the minimum angle 
      IF((DEND/GDFLX(IA,K) - 0.9) .LT. 0) GOTO 75    !less than one hop
c     good mode
      FOUND = .TRUE.
      GOTO 80 
c     bad mode      
   75 HPFLX(IA,K) = 0 
   80 CONTINUE
      if (FOUND) GOTO 95
c<<<<<       
C  PENETRATED LAYER, TRY OVER THE MUF MODE.
C.....SO IF NONE FAILS, INCLUDE OVER-THE-MUF
      IM = MODMUF
      DELFX(1,K) = DELMUF(IM)
      HPFLX(1,K) = HPMUF (IM)
      HTFLX(1,K) = HTMUF (IM)
      FVFLX(1,K) = FVMUF (IM)
      AFFLX(1,K) = AFMUF (IM)
      XHOP = NHOPMF(IM)
      GDFLX(1,K) = GCDKM/XHOP
      IMODE(1,K) = MODMUF
   95 CONTINUE
c>>>>>A.S. 
      BC = (FREQ+ GYZ(LX(K)))**1.98
c<<<<< 
      AC = 677.2*ABIY(K)
      DO 250 IA = 1,45
C.....PRESET ARRAYS
      TGAINX(IA,K) = -10.
      ANDVX(IA,K) = 1000.
      ADVX (IA,K) = 1000.
      GRLOSX(IA,K) = 0.0
C.....MODE GOOD (QUESTION MARK)  ONE IS.
ccc      write(12,14) ia,k,hpflx(ia,k),delfx(ia,k)
ccc14    format('ia,k,hpflx,delfx=',2i5,2f10.4)
      IF(HPFLX(IA,K).lt.70.) go to 250
C.....CHECK FOR MINIMUM
c>>>>>A.S. 
c      this line was causing problems and has been removed
c      no need to check DELFX 
c      since all bad modes now have HPFLX = 0
c      IF(DELFX(IA,K).lt.AMIND) go to 250
c<<<<<       
      DEL = DELFX(IA,K) * D2R      !  radiation angle
      CDEL = COS(DEL)
C.....E MODE (QUESTION MARK)
      IF(FI(1,K).lt.FVFLX(IA,K) ) go to 125
C  D-E  LAYER MODES.
C.....VERY LOW REFLECTIONS (QUESTION MARK)
      IF(HTLOSS.le.HTFLX(IA,K)) then
         XNSQ = 10.2
      else
         HNUX = 61.+ 3.*(HTFLX(IA,K)- 70.)/18.
C.....COLLISION FREQUENCY
         XNSQ = XNUZ * EXP(-2.*(HNUX -60.)/HNU)
      end if
      HEFF = AMIN1(100.,HTFLX(IA,K))
      SINP = RZ* CDEL/(RZ+ HEFF)
      SECP = 1./SQRT(1.- SINP*SINP)
C.....ABSORPTION LOSS
      ANDVX(IA,K)= SECP *AC /(BC +XNSQ)
      XV = AMAX1(FVFLX(IA,K)/FI(1,K) , XVE)
C.....CORRECTION FOR E LAYER
      ADX= AFE + BFE* ALOG(XV)
      SINP = RZ*CDEL/(RZ +HPFLX(IA,K))
      SECP = 1./SQRT(1.-SINP*SINP)
C.....DEVIATIVE LOSS + CORRECTION
c>>>>>A.S. 
      ADVX(IA,K) = SECP*AFFLX(IA,K)*( ( FVFLX(IA,K)+GYZ(LX(K)))**1.98
     A +XNSQ) /(BC+ XNSQ)   +ADX
c<<<<< 
C.....ES OBSCURATION, NONE
      AOFX(IA,K) = 0.0
      GO TO 160
C  F LAYER MODES
  125 CONTINUE
      XNSQ = 10.2
      SINP = RZ*CDEL/(RZ +100.)
      SECP = 1./SQRT(1. -SINP*SINP)
C.....ABSORPTION LOSS
      ANDVX(IA,K) = SECP*AC/(BC+ XNSQ)
      SINP = RZ* CDEL/(RZ +HPFLX(IA,K))
      SECP = 1./SQRT(1. -SINP*SINP)
C.....DEVIATIVE LOSS
c>>>>>A.S. 
      ADVX(IA,K) = SECP*AFFLX(IA,K)*((FVFLX(IA,K)+GYZ(LX(K)))**1.98 
     A +XNSQ)/(BC+ XNSQ)
c<<<<< 
C  ES OBSCURTION LOSS
      AOFX(IA,K) =0.0
C.....SPORADIC E EXISTS (QUESTION MARK)
      IF(FS(2,K).le.0.) go to 160
      FSDEAD =IFOB(1,3,K)
      FSDEAD =FSDEAD/1000.
      FMHZ = AMAX1(FREQ,FSDEAD)
      SINS = RZ*CDEL/(RZ + HS(K) )
      SECS = 1./SQRT(1.-SINS*SINS)
C.....MUF THIS DISTANCE
      ESD  = FS(2,K)*SECS
      DUMMY = YMUF(4)
      P = PRBMUF(FMHZ,ESD,DUMMY,4)
      P = AMAX1(.1,P)
      P = AMIN1(.9,P)
C.....OBSCURATION
      AOFX(IA,K) = -10.*ALOG10(1.-P)
  160 CONTINUE
C GROUND LOSS
      Y = 0.0
      DO 170 IG = 1,KM
      IGX = -IG
      CALL GAIN(IGX,DEL,FREQ,YG,GEFF)
  170 Y = Y + YG
      XKM = KM
C.....AVERAGE GROUND LOSS
      GRLOSX(IA,K) = Y / XKM
C ANTENNA GAINS
C125PC      IF(MSPEC.EQ.125)THEN
C125PC        TGAINX(IA,K) =0.
C125PC      ELSE
        CALL GAIN(JJ,DEL,FREQ,Y,TEFF)
        TGAINX(IA,K) =Y
        if(JJ.eq.2)EFFlp(ia)=TEFF
C125PC      ENDIF
      SPHET = RZ * CDEL / (RZ + HTFLX(IA,K))
      CPHET = 1.-SPHET*SPHET
      CPHET = AMAX1(0.00000001,CPHET)
      CPHET = SQRT(CPHET)
C-----?????????????????
      IS = IMODE(IA,K)
      IS = MODMUF
C-----?????????????????
      DUMMY = YMUF(IS)
      PROS = PRBMUF(FREQ,DUMMY,DUMMY,IS)
C.....OVER-THE-MUF, MEDIAN
      XLS = -10.*ALOG10(PROS)/CPHET
      IF(IAND(INFO,2).GT.0)THEN
        XLSWO=XLS*CPHET
        WRITE(99,'(2(A,I2),A,2F7.2,A,2F7.2)')' "SETTXR" LAY=',IS,
     1'  IA=',IA,'  FRQ,YMUF=',FREQ,DUMMY,'   MD OTMs,OTM=',XLS,
     2  XLSWO
      ENDIF
C ADD ADJUSTMENT TO ABSORPTION LOSS
      ANDVX(IA,K) = ANDVX(IA,K) + XLS
      PROB(JJ) = PROS
      ILN=ILN+1
C125PC      IF(MSPEC.EQ.125)THEN
C125PC      itlos(ILN,JJ,IFQN,IUR,ISZ,ISP)=(andvx(ia,k)+advx(ia,k)+.05)*10.
C125PC      imod=IMODE(ia,k)
C125PC      imddl(ILN,JJ,IFQN,IUR,ISZ,ISP)=imod*10000+(delfx(ia,k)+.05)*10.
C125PC      ihpfx(ILN,JJ,IFQN,IUR,ISZ,ISP)=(hpflx(ia,k)+.05)*10.
C125PC      igdfx(ILN,JJ,IFQN,IUR,ISZ,ISP)=(GDFLX(IA,K)+.5)
C125PC      ENDIF
  250 CONTINUE
C125PC      IF(MSPEC.EQ.125)LNANG(JJ,IFQN,IUR,ISZ,ISP)=ILN
 1000 CONTINUE
      RETURN
      END
C--------------------------------
