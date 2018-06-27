c###luffy.for
      SUBROUTINE LUFFY(idump)
C.....VERSION 11.JAN.95
C.....VERSION 10.APRIL.92
      LOGICAL LHOP
      CHARACTER*4 NAMES,APW
      CHARACTER*40 VERSN
      common /chours/ nhours,ihours(24)
      COMMON /CON/D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON/MUFS/ EMUF(24,3),F2MUF(24,6),ALLMUF(24),EFMUF(24),FOT(24)
     A, FOT1(24),XLUF(24),ZLUF(24),OPMUF(24),OPMUF1(24),FU(24),FL(24)
     B, KFN,LUFC,NLOOP,ACT1,DXL,XMODE,FX,PR
      COMMON/NAMEX/NAMES(20),ISSN,IRED,LINES,LPAGES,MAPIN,KRUN
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      COMMON/GEOG/ABIY(5),CLAT(5),CLONG(5),CLCK(5),EPSPAT(5),F2M3(5)
c>>(b)WP-6A SEPT. 93 MODS add 2 lines REMOVE 1 LINE
     A,FI(3,5),GLAT(5),GMDIP(5),GYZ100(5),GYR100,GYZ300(5),HPF2(5),RD(5)
     B,SIGPAT(5),CYCEN(5)
CX   A,FI(3,5),GLAT(5),GMDIP(5),GYZ(5),HPF2(5),RD(5),SIGPAT(5),CYCEN(5)
c>>(b)WP-6A SEPT. 93 MODS end of change
      COMMON /IPAR/ XF2(3,24),XM3(3,24),XE(5,24),CY24H(5,24),KF
C RAY MODES FOR DISTANCE GHOP,SEE SUBROUTINE CURMUF
      COMMON/MODES/ DELMOD(9),UTMOD(9),ABPS(9),FLDST(9),FSLOS(9)
     1,GRLOS(9),TLOSS(9),XLOSS(9),DELMD1(9),UTMD1(9),KANGLE(12)
     2,DBLOS(12),DBU(12),MODE(12),NHP(12),KANGLA(24,12)
     3,MODEA(24,12),KANGL4(4,12),KDBU4(4,12),DBU4(4,12),MODE4(4,12)
     4,NHP4(4,12),KODEA(24,12)
       character*4 MODE,MODEA,MODE4
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON /D2HOP/ LHOP,NK,KM1,KM2,KM3
C.WP6A-DG5  COMMON USED IN GEOM, LUFFY AND ABSORP
      COMMON /ABSOR/ GYL(5),TAF(5),PEXP(5),ASSN                   ! C.WP6A-DG5
C.WP6A-DG5  SUNSPOT VARIATION OF ABSORPTION
      ASSN=(1.+.0067*SSN)                                         ! C.WP6A-DG5
      IA=0
      IF(KM.GT.1) IA=1
      do 270 ihr=1,nhours
      jt=ihours(ihr)
ccc      DO 270 JJT=IT1,IT2,ITSTEP
ccc      jt=jjt
      if(jt.gt.24) jt=jt-24
ccc      if(jt.lt.IT1) jt=IT1      !  do 1st twice(to get it right)
cccc                               !  actually so GEOM & PARION get called
ccc         if(idump.ne.0) write(*,'(1h+,i2)') jt
C    PRE SET VARIABLES
      EMUF(JT,1)=-1.0
      EMUF(JT,2)=-1.0
      EMUF(JT,3)=-1.0
      F2MUF(JT,1)=-1.0
      F2MUF(JT,2)=-1.0
      F2MUF(JT,3)=-1.0
      F2MUF(JT,4)=-1.0
      F2MUF(JT,5)=-1.0
      F2MUF(JT,6)=-1.0
      ALLMUF(JT)=-1.0
      OPMUF(JT)=-1.0
      IT=JT
      GMT=IT
C.....first estimate of Dmax was the min. value of 3400 km. ,thus
C.....following calc. of mean Dmax in subroutine PARION the content
C.....update contol point infomation,if necessary
      DMX0=DMXA(IT)
      IF(GCDKM.GT.3400.0.AND.GCDKM.LE.DMX0) THEN
       KF=1
       KM=3
C.....place values of characteristics M(3000)F2, foE and foF2
C.....correct locations in accordance with Table I, Rec 533(D < dmax)
       XM3(2,JT)=XM3(3,JT)
       XF2(2,JT)=XF2(3,JT)
        GYL(2)=GYL(3)                                             ! C.WP6A-DG5
        TAF(2)=TAF(3)                                             ! C.WP6A-DG5
        PEXP(2)=PEXP(3)                                           ! C.WP6A-DG5
        GYL(3)=GYL(5)                                             ! C.WP6A-DG5
        TAF(3)=TAF(5)                                             ! C.WP6A-DG5
        PEXP(3)=PEXP(5)                                           ! C.WP6A-DG5
      ELSE IF(GCDKM.GT.DMX0) THEN
       KF=3
       KM=5
      END IF
C.....
      DO 100 II=1,KF
      I3A=II+IA
      FI(3,I3A)=XF2(II,JT)
      HPF5=-176.+1490./XM3(II,JT)
      HPF2(I3A)=AMIN1(HPF5,500.0)
      F2M3(I3A)=XM3(II,JT)
  100 CONTINUE
      CALL TIMVAR
       DO 108 II=1,KM
C......OBTAIN VALUES OF foE,SOLAR ZENITH ANGLE AT EACH CONTROL POINT
       FI(1,II)=XE(II,JT)
       CYCEN(II)=CY24H(II,JT)
  108  CONTINUE
C.WP6A-DG5C.....CALCULATE ABSORPTION
C.WP6A-DG5      DO 106 II=1,KM
C.WP6A-DG5C.....USE ACTUAL SOLAR-ZENITH ANGLE BUT WITH MAX. VALUE OF 102.15 DEGS
C.WP6A-DG5C.... AS 0.881*CHIP < 90 DEG.(eq. for ABSI below)
C.WP6A-DG5C.....HOWEVER,ACUTUAL VALUE OF THIS ANGLE CAN BE USED FOR CALC. OF foE
C.WP6A-DG5C.....BY REC. 434 PROCEDURE (MUGGLETON METHOD)
C.WP6A-DG5      ABSX=(1.0+0.0037*SSN)
C.WP6A-DG5      CHIP=AMIN1(CYCEN(II),102.15)*D2R
C.WP6A-DG5C.....COMPUTE ABSORPTION INDEX,ABSI
C.WP6A-DG5      ABSI=ABSX*COS(0.881*CHIP)**1.3
C.WP6A-DG5      ABIY(II)=ABSI
C.WP6A-DG5  106 CONTINUE
C.....
      CALL CURMUF
      IF(GCDKM.LT.GCDEND) then
         CALL SIGDIS
         CALL DBU252
      end if
C     PRINT LINES IF DESIRED
  270 CONTINUE
      IF(GCDKM.LE.GCDFTZ) return
      do 370 ihr=1,nhours
      jt=ihours(ihr)
      if(jt.gt.24) jt=jt-24
      IT=JT
      GMT=IT
      CALL FTZ
  370 CONTINUE
      RETURN
      END
c--------------------------------------------
