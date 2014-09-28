c###regmod.for
      SUBROUTINE REGMOD
C---------------------------------
C
C     THIS ROUTINE FINDS ALL MODES FOR THE CURRENT FREQUENCY AND THE
C     DISTANCE FROM THE IONOGRAM (NO SPORADIC E MODES - SEE SUBROUTINE
C     ESMOD) THIS IS FOR A GIVEN HOP DISTANCE SET IN SUBROUTINE LUFFY
c
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SXGU,SXGL,KJ,JK
      COMMON/SIGD/DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE,HNU
     A ,HTLOSS,XNUZ,XVE
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24),FOT
     A(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMUF(4)
     B ,HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     C ,YMUF(4)
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON / ZON / ABPS(7), CREL(7), EFF(7), FLDST(7), GRLOS(7),
     1 HN(7), HP(7), PROB(7), RELY(7), RGAIN(7), SIGPOW(7), SN(7),
     2 SPRO(7), TGAIN(7), TIMED(7), TLOSS(7), B(7), FSLOS(7), ADV(7),
     3 OBF(7),NMODE(7),TLLOW(7),TLHGH(7)
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS
     A ,SPR,SU,SUS,XNOISE,ZNOISE,NF
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON /ES /FS (3, 5), HS (5)
      COMMON /MODES /GHOP, DELMOD (6, 3), HPMOD (6, 3), HTMOD (6, 3), FV
     1MOD (6, 3), ITMOD (6, 3), AFMOD (6, 3)
      COMMON /GEOG /GYZ (5), RAT (5), GMDIP (5), CLCK (5), ABIY (5), ART
     1IC (5), SIGPAT (5), EPSPAT (5)
      COMMON / RAYS / ANG(40), IFOB(40,30,5), NANG
      COMMON/INFORM/INFO,IHSHR,IHLNG
      DIMENSION LX(3)
      DATA LX/1,3,5/
C
C  TEMPORARILY FILL  A SELECTED MODES INTO COMMON/ZON
C  SHOULD DO THIS BY A EQUAL TAKEOFF ANGLE SEARCH IN COMMON/REFLX/
C  WILL USE  COMMON/MODES/ NOW. THIS IS A ONE SAMPLE AREA FOR CORRECT
C  DISTANCE  AT ALL SAMPLE AREAS. FOR A SHORT PATH THESE RESULTS ARE
C  CORRECT. MODES THAT GO ALL SAMPLE AREAS ARE GOOD. BUT SOME GOOD MODES
C  MAY BE MISSED THIS WAY.
C  SELECT SAMPLE AREA TO BE USED BY CRITICAL FREQUENCIES.
C  SET BY CALL TO SUBROUTINE SANG FROM SUBROUTINE LUFFY
C  RELATE 3 IONOSPHERES TO 5 SAMPLE AREAS
      K = JMODE
      L = LX(K)
C.....USE AVERAGE ABSORBTION INDEX FROM SUBROUTINE SIGDIS
      AC = 677.2 * ACAV
      BC = (FREQ + GYZ(L) ) ** 1.98
C.....IHOP IS THE NUMBER OF HOPS (GHOP IS FROM SUBROUTINE LUFFY)
      IHOP = GCD/GHOP  +.01
      HOP=IHOP
C  NOW ADD LOSSES
      DO 240 IM = 1,7
      ITRY =1
C.....PRESET IN COMMON/ZON/
      HN(IM) = -1.
      HP   (IM) = -1.
      IF(IM.ge.7) go to 240
      IF(ITRY.lt.1) go to 240
      IF(HPMOD(IM,K).le.0.) go to 240
C   FREE SPACE LOSS LOSS RELATIVE TO AN ISOTROPIC RADIATOR
      DEL = D2R * DELMOD (IM, K)
      DEL = AMIN1(DEL, 89.99*D2R)
      CDEL = COS (DEL)
      PSI = GHOP * 0.5
      PHE = PIO2 - PSI - DEL
      PATH = 2. * (HPMOD(IM,K) + RZ * (1. - COS(PSI))) / COS(PHE)
      PATH=ABS(PATH*HOP)
C.....TIME DELAY
      TIMED(IM)=PATH/VOFL
C.....FREE SPACE LOSSES
      FSLOS (IM) = 32.45 + 20.*ALOG10(PATH*FREQ)
      IF(FI(1,K) - FVMOD(IM,K))  125,106,106
C  D-E MODE
  106 CONTINUE
      IF(HTLOSS - HTMOD(IM,K) )  110,110,115
C.....XNSQ IS THE COLLISION FREQUENCY TERM
  110 XNSQ = 10.2
      GO TO 120
  115 HNUX= 61. + 3.*(HTMOD(IM,K) - 70.)/18.
      XNSQ=  XNUZ * EXP(- 2.*(HNUX-60.)/HNU)
  120 CONTINUE
C.....SECANT PHE FOR REFLECTION AREA - THE CLASSICAL HEIGHT OF 100 KM
C.....IS ASSUMED FOR HTMOD .GT. 100 KM
      HEFF = AMIN1( 100.,HTMOD(IM,K))
      SINP = RZ*CDEL/(RZ+ HEFF)
      SECP = 1./SQRT(1. -SINP*SINP)
      ABPS(IM) = SECP* AC/(BC + XNSQ)
C.....ABSORPTION LOSS BUT REMOVE E LAYER BENDING EFFECT
      XV = AMAX1( FVMOD(IM,K)/FI(1,K) , XVE)
      ADX =  AFE + BFE * ALOG(XV)
      SECP= 1./SIN(DEL +PSI)
       BCX = BC
C.....DEVIATION TERM FOR HIGH ANGLE RAYS, PLUS E LAYER BENDING EFFECT
      ADV(IM) = SECP*AFMOD(IM,K)*((FVMOD(IM,K)+GYZ(L))**1.98 + XNSQ)
     A          / (BCX + XNSQ ) + ADX
C.....SET ES OBSCURATION TO 0.0 FOR D - E MODES
      OBF(IM) = 0.0
      OBFU = 0.0
      OBFL = 0.0
      GO TO 160
C   F LAYER MODES
  125 CONTINUE
C.....COLLISION FREQUENCY TERM
C.....HTMOD .GT. 100KM FOR F LAYER
      XNSQ = 10.2
      SINP = RZ*CDEL/(RZ + 100.)
      SECP = 1./SQRT(1.-SINP*SINP)
C.....ABSORPTION LOSS
      ABPS(IM)= SECP*AC/(BC+ XNSQ)
      SECP = 1./SIN(DEL + PSI)
C.....DEVIATIVE LOSS TERM FOR HIGH ANGLE MODES
      ADV(IM) = SECP* AFMOD(IM,K)*((FVMOD(IM,K) +GYZ(L))**1.98 + XNSQ)
     A          /(BC + XNSQ)
C  ES OBSCURATION LOSS
      OBF(IM) = 0.0
      OBFU = 0.0
      OBFL = 0.0
      IF(FS(2,K).le.0.) go to 160
C.....FOR LOW FREQUENCIES, INCLUSION OF REGULAR E INTO SPORADIC E CAUSES
C.....PROBLEM, SEE ALSO SUBROUTINE SIGDIS
      FSDEAD = IFOB(1,3,K)
      FSDEAD = FSDEAD/1000.
      FSDEAD = AMIN1(FSDEAD,3.)
      FMHZ = AMAX1(FREQ,FSDEAD)
      SINS = RZ*CDEL/(RZ+ HS(K))
      SECS = 1./SQRT(1.- SINS*SINS)
C.....MEDIAN MUF FOR THIS HOP, NOT FOR THE PATH.
C.....PRBMUF IS THE PROBABILITY FUNCTION
C.....NOTE LIMIT ON LOSS
      ESD  = FS(2,K)*SECS
      DUMMY = YMUF(4)
      PROS = PRBMUF(FMHZ,ESD,DUMMY,4)
      PROS = AMIN1(PROS,.90)
      OBF(IM) = -10.*ALOG10(1.-PROS)
      ESD = FS(1,K)* SECS
C.....UPPER DECILE OBSCURATION AT ES FOT
      DUMMY = YFOT(4)
      PROS = PRBMUF(FMHZ,ESD,DUMMY,4)
      PROS = AMIN1( PROS, 0.90)
      OBFU= -10.*ALOG10(1.- PROS)
C.....LOWER DECILE OBSCURATION AT ES HPF
      DUMMY = YHPF(4)
      PROS = PRBMUF(FMHZ,ESD,DUMMY,4)
      PROS = AMIN1(PROS, 0.9)
      OBFL =  -10.*ALOG10(1.- PROS)
  160 CONTINUE
      Y = 0.0
      DO 185 IG = 1,KM
      IGX = - IG
      CALL GAIN(IGX,DEL,FREQ,YG,GEFF)
  185 Y = Y + YG
      XKM = KM
C.....AVERAGE GROUND LOSS
      GRLOS(IM) = Y / XKM
C  ANTENNA GAINS
C125PC      if(MSPEC.ne.125)then
C.......GAIN AT TRANSMITTER
        CALL GAIN(1,DEL,FREQ,DUMMY,TEFF)
        TGAIN(IM) = DUMMY
C.......GAIN AT RECEIVER
        CALL GAIN(2,DEL,FREQ,DUMMY1,DUMMY2)
        RGAIN(IM) = DUMMY1
        EFF(IM) = DUMMY2
C125PC      else
c.......set gains and EFF to 0. dB or unity
C125PC        tgain(im)=0.
C125PC        rgain(im)=0.
C125PC        EFF(im)=0.
C125PC      ENDIF
C.....NOTE ONLY 2 HOPS FOR THE OBSCURATION
      HOPS = AMIN1( HOP, 2.)
      XTLOS = FSLOS(IM) + HOP*(ABPS(IM)+ADV(IM)) + GRLOS(IM)*(HOP-1.)
     A  + HOPS*OBF(IM) + ASM - RGAIN(IM) - TGAIN(IM)
ccc      write(luo,123) xtlos,im,fslos(im),hop,abps(im),adv(im),grlos(im),
ccc     +               obf(im),asm
ccc123   format('xtlos=',f9.2,i4,7f9.2)
C.....BEGINNING OF TRANSMISSION LOSS DISTRIBUTION
      ISMOD = ITMOD(IM,K)
      SPHET = RZ * CDEL / (RZ + HTMOD(IM,K))
      CPHET = 1. - SPHET * SPHET
      CPHET = AMAX1(0.000001,CPHET)
      CPHET = SQRT(CPHET)
C.....MUF FOR THIS HOP DISTANCE
c%lc:gsp, change 1a     12 Jan 1995,                        subroutine  regmod
c%lc    "prob()" is the array to be printed as fdays in methods 16-23, and 
c%lc    "MODE PROB" in method 25.  This change is to base fdays on the specific
c%lc    hop muf, not path muf.  This change supercedes fdays code below.  See 
c%lc    change 1b.  Note: the code above for CPHET is problematic and does not
c%lc    give a correct "hop muf." It may be correct for its other uses, though.
c%lc    A better "hop muf" is calculated in this added code.  The original is 
c%lc    preserved for its original uses.
c%lc begin change 
      PSI = GCD/2.
      PSI=PSI/HOP
      CPSI = COS(PSI)
      SPSI = SIN(PSI)
      TANP = SPSI / (1. - CPSI + HPmuf(ismod) / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HTmuf(ismod))
      xmuf=fvmuf(ismod)/sqrt(1.-sphe*sphe)
      DUMMY = YMUF(ISMOD)
      Prob(im) = PRBMUF(FREQ,XMUF,DUMMY,ISMOD)   !  this is MUFday
c          add more loss when MUFday gets very low but put a limit on it.
c          Extra loss goes from 0 to 24dB for MUFday .0001 to .0000001
c          this was added 11/11/2006 for problems posed by Jim Tabor
      if(prob(im).lt..0001) then
         xfac=alog10(prob(im))
         if(xfac.gt.-4.) xfac=-4.
         if(xfac.lt.-7.) xfac=-7.
         ghlos=(xfac+4.)*8.
         xtlos=xtlos-ghlos
      end if
c%lc. end change 1a
      XMUF = FVMUF(ISMOD)/CPHET
      DUMMY = YMUF(ISMOD)
ccc      write(luo,'(''before prbmuf='',3f12.6,i5)') freq,xmuf,dummy,ismod
      P = PRBMUF(FREQ,XMUF,DUMMY,ISMOD)
      if(p.le..000001) p=.000001         !  put a limit on low probability (GRH 5/12/2005)
      XLS =  -10.*ALOG10(P)/CPHET
ccc      write(luo,'(''xls='',3f12.6)') xls,p,cphet
      IF(IAND(INFO,2).GT.0)THEN
        XRAT=XMUF/DUMMY
        XLSWO=XLS*CPHET
        WRITE(99,'(13h "REGMOD" LY=,I1,19h FRQ,XMUF,YMUF,RAT=,3F6.2,
     1  F5.2,11h  OTMs,OTM=,2F6.2)')ISMOD,FREQ,XMUF,DUMMY,XRAT,XLS,XLSWO
      ENDIF
C.....MEDIAN
      XTLOS = XTLOS + XLS*HOP
ccc      write(luo,'(''xtlos='',3f10.3)') xtlos,xls,hop
      CPR = FVMUF(ISMOD)/YMUF(ISMOD)
      FVFOT = YFOT(ISMOD)* CPR
      XMUF  = FVFOT/ CPHET
      DUMMY = YFOT(ISMOD)
      PF = PRBMUF(FREQ,XMUF,DUMMY,ISMOD)
      if(pf.le..000001) pf=.000001       !  put a limit on low probability (GRH 5/12/2005)
      XLSL = -10.*ALOG10(PF)/CPHET
      FVHPF = YHPF(ISMOD)*CPR
      XMUF  =  FVHPF/CPHET
      DUMMY = YHPF(ISMOD)
      PF = PRBMUF(FREQ,XMUF,DUMMY,ISMOD)
      if(pf.le..000001) pf=.000001       !  put a limit on low probability (GRH 5/12/2005)
      XLSU = -10.*ALOG10(PF)/CPHET
      IF(IAND(INFO,2).GT.0)THEN
        XLSLWO=XLSL*CPHET
        XLSUWO=XLSU*CPHET
        WRITE(99,'(2(5X,A,2F7.2))')'L-DECILE} OTMs,OTM=',XLSL,XLSLWO,
     1 'U-DECILE} OTMs,OTM=',XLSU,XLSUWO
      ENDIF
C
C DECILES OF SIGNAL LEVEL
C
c%lc:gsp, comment change 12 Jan 1995,                      subroutine  regmod
c%lc Despite the name, tllow is the signal level - lower decile; tlhgh is the
c%lc signal level - upper decile.
      TLLOW(IM) =  DSL + HOPS *(OBFL-OBF(IM)) + HOP*(XLSL - XLS)
      TLHGH(IM) =  DSU + HOPS *(OBF(IM)-OBFU) + HOP*(XLS  - XLSU)
      TLLOW(IM) = AMIN1(TLLOW(IM) , 25.)
      TLHGH(IM) = AMIN1(TLHGH(IM) , 25.)
C.....ENDING OF TRANSMISSION LOSS DISTRIBUTION
C
C.....THIS IS F.DAYS (NOT USED ELSEWHERE)
c%lc:gsp, change 1b     12 Jan 1995,                       subroutine  regmod
c%lc      the next two lines have been superceded by change 1a above.
c%lc            DUMMY = YMUF(ISMOD)
c%lc            PROB(IM) = PRBMUF(FREQ,DUMMY,DUMMY,ISMOD)
c%lc. end change 1b
      ITRY = -1
      TLOSS (IM) = XTLOS
C.....FLDST(IM) IS FIELD STRENGTH
C.....SIGPOW(IM) IS SIGNAL
C.....SN(IM) IS SIGNAL TO NOISE
C.....B(IM) IS RADIATION ANGLE
C.....NMODE(IM) IS MODE
C.....HP(IM) IS VIRTUAL HEIGHT
C.....HN(IM) IS NUMBER OF HOPS
      FLDST(IM) = 107.2 + PWRDB(FREQ) + 20.*ALOG10(FREQ)-XTLOS-RGAIN(IM)
      SIGPOW (IM) = PWRDB(FREQ) - XTLOS
C------------    CHANGED 9/24/91  (LONG PATH RCVR EFF CORRECTION)  FJR
C------------          SN(IM) = SIGPOW(IM) - RCNSE
      SN(IM) = SIGPOW(IM) - RCNSE - EFF(IM)
      B (IM) = DELMOD(IM,K)
      NMODE(IM) = ISMOD
      HP(IM) = HPMOD(IM,K)
      HN(IM)   = IHOP
  240 CONTINUE
      RETURN
      END
C--------------------------------
