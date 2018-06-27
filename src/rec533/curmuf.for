      SUBROUTINE CURMUF
C----------------------------------------------------------------------
C     Version 03.Nov.93
C----------------------------------------------------------------------
      LOGICAL LHOP
      INTEGER*4 ANTREF
      REAL*4 SECE(3),XMF(6)
      CHARACTER*39 DESC
      CHARACTER*40 VERSN
      common /chours/ nhours,ihours(24)
      COMMON / CON / D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON /FRQ/ FREL(12),FREL5(11,7),FW5(11,7),MAXF,MF,KOF,FREQ
     A, JKFMIN,FXEMAX
      COMMON/GEOG/ABIY(5),CLAT(5),CLONG(5),CLCK(5),EPSPAT(5),F2M3(5)
c>>(b)WP-6A SEPT. 93 MODS add 2 lines REMOVE 1 LINE
     A,FI(3,5),GLAT(5),GMDIP(5),GYZ100(5),GYR100,GYZ300(5),HPF2(5),RD(5)
     B,SIGPAT(5),CYCEN(5)
CX   A,FI(3,5),GLAT(5),GMDIP(5),GYZ(5),HPF2(5),RD(5),SIGPAT(5),CYCEN(5)
c>>(b)WP-6A SEPT. 93 MODS end of change
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON /D2HOP/ LHOP,NK,KM1,KM2,KM3
      COMMON /SOL/ DECL12(12),EQT12(12),DECL,EQT,MONTH,IMON(12)
C RAY MODES FOR DISTANCE GHOP,SEE SUBROUTINE CURMUF
      COMMON/MODES/ DELMOD(9),UTMOD(9),ABPS(9),FLDST(9),FSLOS(9)
     1,GRLOS(9),TLOSS(9),XLOSS(9),DELMD1(9),UTMD1(9),KANGLE(12)
     2,DBLOS(12),DBU(12),MODE(12),NHP(12),KANGLA(24,12)
     3,MODEA(24,12),KANGL4(4,12),KDBU4(4,12),DBU4(4,12),MODE4(4,12)
     4,NHP4(4,12),KODEA(24,12)
       character*4 MODE,MODEA,MODE4
      COMMON/MUFS/ EMUF(24,3),F2MUF(24,6),ALLMUF(24),EFMUF(24),FOT(24)
     A,FOT1(24),XLUF(24),ZLUF(24),OPMUF(24),OPMUF1(24),FU(24),FL(24)
     B, KFMINN,LUFC,NLOOP,ACT1,DXL,XMODE,FX,PR
      COMMON / HOG / IANTT, BANTT, IANTR, BANTR
      COMMON /A705/ IANT,IANTQ,ANTREF(2),DESC(2),G705(2,29,91),IFREQ
     1, DESFRQ(2),DESRAN(2,2),DSTXT(2,13),FLOW,FHIGH
C.....
c>>(e)WP-6A SEPT. 93 MODS modifications as described in next 3 lines
C.....KEMIN, KEMAX integer indicating control point location where foE is
C.....is a mininum, maxinum resp.
C.....KFMIN, KEMAX integer indicating control pt.location where foF2 is a min.
C.....RECEIVER SITE SAMPLE AREA (KM = MAX. NO. OF CONTROL POINTS)
      IF(KM-3)  100,105,110
C.....ONE SAMPLE
  100 KEMIN=1
      KEMAX=1
      KFMIN=1
      GO TO 117
C.....THREE SAMPLES
  105 KFMIN=2
      KEMIN=1
      KEMAX=3
      IF(FI(1,1)-FI(1,3)) 117,117,106
  106 KEMIN=3
      KEMAX=1
      GO TO 117
C.....FIVE SAMPLES
C.....SEPARATE TESTS FOR SELECTION OF LOWEST CRITICAL FREQUENCIES
  110 KEMIN=1
      KEMAX=5
      IF(FI(1,1)-FI(1,5)) 115, 115, 112
  112 KEMIN=5
      KEMAX=1
  115 KFMIN=2
      IF(FI(3,2)-FI(3,4)) 117, 117, 116
  116 KFMIN=4
  117 CONTINUE
C
C.....E LAYER MUF
C
C.....SEE SUBROUTINE LUFFY FOR FOE CALCULATION (REP 894)
      FXE=FI(1,KEMIN)
      FXEMAX=FI(1,KEMAX)
C     DO DISTANCE VARIATION FIRST HOUR ONLY
      IF(IT.NE.ihours(1)) GO TO 125
      HPE=110.
C.....A MIN. ELEV. ANGLE OF 3 DEGREES[0.05236 RADIANS,COS(DEL)=0.99863]
C.....IS USED TO DETERMINE MODE STRUCTURE AND MODE BASIC MUF'S
      DELE=3.0
      DEL=0.052360
      CDEL=0.99863
      PHE=ASIN(RZ*CDEL/(RZ+HPE))
      NHOPS=IFIX(0.5 * GCDKM / ((PIO2-DEL - PHE) * RZ))
      IE=1
      EHOPS=FLOAT(NHOPS)
  118 EHOPS=EHOPS+1.0
      PSI=GCDKM/((2.*RZ)*EHOPS)
      CPSI=COS(PSI)
      SPSI=SIN(PSI)
      TANP=SPSI / (1.-CPSI+HPE / RZ)
      PHE=ATAN(TANP)
      DEL=PIO2-PHE - PSI
      CDEL=COS(DEL)
      SINE=RZ*CDEL/(RZ+ HPE)
      SINE=AMAX1(0.000001,SINE)
      SECE(IE)=1.0/SQRT(1.0-SINE*SINE)
      DELE=DEL * R2D
      DELMOD(IE)=DELE
      UTMOD(IE)=EHOPS
C.....COPY ELEV. ANGLE,NO. HOPS INTO ARRAYS DELMD1,UTMD1
      DELMD1(IE)=DELE
      UTMD1(IE)=EHOPS
      IE=IE+1
      IF(IE.LE.3) GO TO 118
  125 CONTINUE
      DO 126 L=1,3
126   EMUF(IT,L)=FXE*SECE(L)
C.... F2  LAYER MUF
      JKFMIN=KFMIN
C.....SET ZENITH ANGLE TO DETERMINE OPERATIONAL MUF FACTOR
      ZENANG=CYCEN(KFMIN)
      HP2=HPF2(KFMIN)
C.....MIN. ELEV.=3 DEGREE
      DEL2=3.0
      DEL=0.052360
      CDEL=0.99863
      PHE=ASIN(RZ*CDEL/(RZ+HP2))
C.....NOTE RE-FORMULATION OF ABOVE EQUATION
      NHOPS=IFIX(0.5 * GCDKM / ((PIO2-DEL - PHE) * RZ))
      NHOPS1=NHOPS+1
C.....NOTE SIMPLIFICATION OF FOLLOWING STRUCTURE -note no longer
C.....attepmpt to iterate to no. of hops
      LHOP=.FALSE.
      IF1=4
      FHOPS=FLOAT(NHOPS1)
  128 CONTINUE
      IF2=IF1-3
      GCDHOP=GCDKM/FHOPS
      PSI=GCDKM/((2.*RZ)*FHOPS)
      CPSI=COS(PSI)
      SPSI=SIN(PSI)
      TANP=SPSI / (1.-CPSI+HP2/RZ)
      PHE=ATAN(TANP)
      DEL=PIO2-PHE - PSI
       FOF2=FI(3,KFMIN)
       FOE=FI(1,KFMIN)
       XR=FOF2/FOE
       XM3=F2M3(KFMIN)
c>>(b)WP-6A SEPT. 93 MODS CHANGE 'GYZ' to 'GYZ300'
       FH=GYZ300(KFMIN)
C......CALC. F2-MUF USING THE Rec. 434 PROCEEDURE
       CALL MUF434(FOF2,XR,XM3,FH,GCDHOP,IF2,KFMIN,DMXF,F2DMUF)
       F2MUF(IT,IF2)=F2DMUF
C..... STORE MUF FACTORS IN ARRAY 'XMF'
       XMF(IF2)=F2MUF(IT,IF2)/(FOF2+.5*FH)
C......FOR PATHS GREATER THAN DMX0
      IF(GCDKM.LE.DMX0) GO TO 160
C..... CALC. MUF(DMX0)F2  AT BOTH CONTROL POINTS
C..... DETERMINE LOWER OF F2(DMX)MUF AND MUF FACTOR ('XMF..) VALUES
C......AS CALCULATED CONTROL POINTS (TABLE 1(i))
       FOF22=FI(3,2)
       FOE2=FI(1,2)
       XR2=FOF22/FOE2
       XM32=F2M3(2)
       FH2=GYZ300(2)
c>>(b)WP-6A SEPT. 93 MODS CHANGE 'GYZ' to 'GYZ300'
       CALL MUF434(FOF22,XR2,XM32,FH2,DMX0,IF2,2,DMXF,F4)
       XMF4=F4/(FOF22+.5*FH2)
       FOF24=FI(3,4)
       FOE4=FI(1,4)
       XR4=FOF24/FOE4
       XM34=F2M3(4)
c>>(b)WP-6A SEPT. 93 MODS CHANGE 'GYZ' to 'GYZ300'
       FH4=GYZ300(4)
       CALL MUF434(FOF24,XR4,XM34,FH4,DMX0,IF2,4,DMXF,F44)
       XMF44=F44/(FOF24+.5*FH4)
       F2MUF(IT,IF2)=AMIN1(F4,F44)
       XMF(IF2)=AMIN1(XMF4,XMF44)
C......STORE PATH BASIC MUF IN 'PBMUF'
       IF(IF2.EQ.1) PBMUF=F2MUF(IT,IF2)
  160 CONTINUE
      IF(GCDKM.LE.DMX0.OR.IF2.EQ.1) GO TO 170
C.....CALC. MODE MUFS FOR HIGHER ORDER MODES AS A FRACTION OF MUF
C.....FOR LOWEST ORDER MODE(=PATH BASIC MUF)
      F2MUF(IT,IF2)=XMF(IF2)/XMF(1)*PBMUF
  170 CONTINUE
      DEL2=DEL * R2D
      DELMOD(IF1)=DEL2
      UTMOD(IF1)=FHOPS
      DELMD1(IF1)=DEL2
      UTMD1(IF1)=FHOPS
C.....ADD A HOP FOR HIGHER ORDER MODE
      IF1=IF1+1
      FHOPS=FHOPS+1.
      IF(IF1.LE.9) GO TO 128
C.....CIRCUIT MUF AND FOT (OWF)
      IF(GCDKM.GT.4000.) EMUF(IT,1)=0.0
      ALLMUF(IT)=AMAX1(EMUF(IT,1),F2MUF(IT,1))
C.....DETERMINE OPERATIONAL MUF AS F2MUF * FOP
C.....FROM TABLE 2 OF ANNEX 2, REC. 434
      ELEV=DELMOD(4)
      CALL ANTCALZ(1,F2MUF(IT,1),BANTT,BTRD,ELEV,GAINF)
      EIRP=PWRDB(F2MUF(IT,1))+30.0+GAINF
      OPMUF(IT)=F2MUF(IT,1) * FOP(MONTH,ZENANG,EIRP)
C.....CALC. FOT(OWF),LOWER DECILE OF THE OPERATIONAL MUF
c*********************************************************************
c          Change made by Greg Hand 13 Jan 2005 requested by ITU
c          to modify the path operational MUF using ITU-R P.1240
ccc      p1240_fact=p1240(ssn,month,gmt,plat,plong)  !  get factor from ITU-R P.1240 Table 2
ccc      FOT(IT)=AMAX1(EMUF(IT,1)*0.95,OPMUF(IT)*p1240_fact)
cccccc      FOT(IT)=AMAX1(EMUF(IT,1)*0.95,OPMUF(IT)*0.85)    !  this is how it was before
c*********************************************************************
c          As of 14 Dec 2006, no longer use P.1240.
c          Now use P.1239 Table 2 for lower decile
      call get_mid_point(elat,elong)    !  get path midpoint
      xlt=amod(gmt+elong/15.,24.)       !  convert to local time
      call get_fof2_var(xlt,elat,month,ssn,dl_v,du_v)
      FOT(IT)=AMAX1(EMUF(IT,1)*0.95,OPMUF(IT)*dl_v)
c*********************************************************************
      OPMUF(IT)=AMAX1(OPMUF(IT),EMUF(IT,1))
      RETURN
      END
C----------------------------------------------------------------------
      subroutine get_mid_point(xlat_mid,xlon_mid)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      COMMON/AZEL/ ZTLAT,ZTLON,ZTHT,ZRLAT,ZRLON,ZRHT,ZTAZ,ZRAZ,
     * ZTELV,ZRELV,ZD,ZDGC,ZTAKOF,ZRAKOF
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      ztlat=tlatd
      ztlon=tlongd
      if(ztlon.lt.0.) ztlon=ztlon+360.
      zrlat=rlatd
      zrlon=rlongd
      if(zrlon.lt.0.) zrlon=zrlon+360.
      ztht=0.
      zrht=0.

ccc      open(73,file='\itshfbc\win32\rec533w\p1240.out')
ccc      rewind(73)
ccc      write(73,'(''get_mid_point='',4f10.4)') ztlat,ztlon,zrlat,zrlon
ccc      close(73)

      call dazel0(ztlat,ztlon,zrlat,zrlon,ztaz,zdgc)     !  get distance
      if(npsl.eq.0) then         !  short path distance
         zdgc=zdgc/2.
      else                       !  long path distance
         zdgc=gcdkm/2.
         ztaz=ztaz+180.          !  go the other way (long way)
         if(ztaz.ge.360.) ztaz=ztaz-360.
      end if

ccc      open(73,file='\itshfbc\win32\rec533w\p1240.out2')
ccc      rewind(73)
ccc      write(73,'(''before dazel1='',4f10.4)') ztlat,ztlon,ztaz,zdgc
ccc      close(73)

      call dazel1     !  find midpoint
      xlat_mid=zrlat
      xlon_mid=zrlon
ccc      write(luo,1) tlatd,tlongd,rlatd,rlongd,xlat_mid,xlon_mid,zdgc,
ccc     +              gcdkm,ztaz
ccc1     format('get_mid_point=',6f10.4,3f10.2)
      return
      end
c----------------------------------------------------------------
