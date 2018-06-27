c###mugfoe.for
      SUBROUTINE MUGFOE
C ------------------------------------------------------
C          SUBROUTINE MUGFOE:
C          CALCULATES FOE CCIR REC. 434 (MUGGLETON METHOD)
C          VERSION 10.APRIL.92
C ------------------------------------------------------
      REAL*4 M
      CHARACTER*40 VERSN
      COMMON /CON/D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON/GEOG/ABIY(5),CLAT(5),CLONG(5),CLCK(5),EPSPAT(5),F2M3(5)
c>>(b)WP-6A SEPT. 93 MODS add 2 lines REMOVE 1 LINE
     A,FI(3,5),GLAT(5),GMDIP(5),GYZ100(5),GYR100,GYZ300(5),HPF2(5),RD(5)
     B,SIGPAT(5),CYCEN(5)
CX   A,FI(3,5),GLAT(5),GMDIP(5),GYZ(5),HPF2(5),RD(5),SIGPAT(5),CYCEN(5)
c>>(b)WP-6A SEPT. 93 MODS end of change
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON /SOL/ DECL12(12),EQT12(12),DECL,EQT,MONTH,IMON(12)
      DATA PLATX/1.57/     !  90 degrees
C.....
      DO 1 JE=1,KM
      PLAT=CLAT(JE)
      PLONG=CLONG(JE)
C.....DCHI=SOLAR ZENITH ANGLE (DEGS) AT LOCAT. PLAT,PLONG (RADIANS))
C.....DCHI12 = LOCAL NOON SOLAR ZENITH VALUE (DEGS)
      DCHI=CYCEN(JE)
      DCHI12=CY12(JE)
      CHI=DCHI*D2R
      CHI12=DCHI12*D2R
      IF(ABS(PLAT).GT.PLATX) PLAT=PLATX*SIGN(1.0,PLAT)
      ADPLAT=ABS(PLAT*R2D)
      CPLAT=COS(PLAT)
C.....LOCAL TIME, TL
c>>(a)WP-6A mods note use of +E longitude
      ELONG=-PLONG
      TL=AMOD(GMT+(ELONG*R2D)/15.0,24.0)
      CHA90=-TAN(PLAT)*TAN(DECL*D2R)
      IF (CHA90.NE.0.0) CHA90=SIGN(AMIN1(ABS(CHA90),1.0),CHA90)
      HA90=ACOS(CHA90)*R2D
c>>(a)WP-6A modification of comments
C.....HA90 = MEAN HOUR ANGLE OF SUNRISE,SUNSET
C.....MEAN LOCAL TIME OF SUNSET, TS
c>>(a)WP-6A MODS now  add 1 line , remove 1 line
c.....TL and TS can be mean time as only difference required
      TS=12.0+HA90/15.0
cx    TS=12.0+( HA90-EQT)/15.0
c>>(a)WP-6A SEPT. 93 MODS end of change
      CCHI=COS(CHI)
      FOE4=0.0
      FOE4M=0.0
C.....CALC. SOLAR ACTIVITY FUNCTION, A
      A=1.0+0.0094*(FLUX-66.0)
      IF(ADPLAT.GE.32.0) THEN
       M=0.11-0.49*CPLAT
       XM=92.0
       YM=35.0
      ELSE
       M=-1.93+1.92*CPLAT
       XM=23.0
       YM=116.0
      END IF
C.....CALC. SEASON FUNCTION, B AND POSITION FUNCTION, C
      B=COS(AMIN1(ABS(CHI12),1.3963))**M     !  1.3963=80 degrees
      C=XM+YM*CPLAT
C.....CALC. TIME-OF-DAY FUNCTION D
C.....FIND P FOR GIVEN LATITUDE RANGE
C....
      IF(ADPLAT.GT.12.0) THEN
       P=1.2
      ELSE
       P=1.31
      END IF
C.....
      IF(DCHI.LT.90.0) THEN
C......DAYTIME
       XCHI=CHI
C......MODIFY CHI IF CHI > 73 DEGS
       IF(DCHI.GT.73.0) XCHI=CHI-(6.27E-13*(DCHI-50.0)**8)*D2R
       D=COS(XCHI)**P
      ELSE
C......NIGHT-TIME
       P72=(0.072)**P
       D2=P72*EXP(25.2-0.28*DCHI)
       IF(DCHI12.GT.90.0) THEN
C.......POLAR WINTER: SUN DOES NOT RISE
        D=D2
       ELSE
C.......H= NO. OF HOURS AFTER SUNSET
        H=AMOD(TL-TS+24.0,24.0)
        D1=P72*EXP(-1.4*H)
        D=AMAX1(D1,D2)
       END IF
      END IF
C.....FOE4 = foE**4 AND FOE4M = MIN. VALUE OF foE**4
      FOE4M=0.004*(1.0+0.021*FLUX)**2
      FOE4=A*B*C*D
      FOE4=AMAX1(FOE4,FOE4M)
      FOE1=SQRT(SQRT(FOE4))
      FI(1,JE)=FOE1
    1 CONTINUE
      RETURN
      END
c------------------------------------------------------------
