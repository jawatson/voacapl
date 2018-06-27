c###bestf.for
      SUBROUTINE BESTF
C.....VERSION 02.DEC.88
C ----------------------------------------------------------------------
C       SUBROUTINE BESTF:
C        SELECTS FREQUENCY FB, (BEST USABLE FREQUENCY,B.U.F)
C        WHERE LUF < FB < OPERATIONAL MUF AND CLOSEST TO THE FOT
C        IF ALL FREQUENCIES < FOT THEN FREQ. CLOSEST TO,BUT BELOW,
C        CHOSEN.
C ----------------------------------------------------------------------
      CHARACTER*8 WRT(8,6),BLANK
      CHARACTER*16 OUTX
      COMMON /FRQ/ FREL(12),FREL5(11,7),FW5(11,7),MAXF,MF,KOF,FREQ
     A, JKF,FXEMAX
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      common /chours/ nhours,ihours(24)
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON/ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON/MUFS/ EMUF(24,3),F2MUF(24,6),ALLMUF(24),EFMUF(24),FOT(24)
     A ,FOT1(24),XLUF(24),ZLUF(24),OPMUF(24),OPMUF1(24),FU(24),FL(24)
     B, KFN,LUFC,NLOOP,ACT1,DXL,XMODE,FX,PR
      DATA BLANK/'        '/
C ----------------------------------------------------------------------
C.....FILL ARRAY WRT WITH BLANKS
      DO 80 JB=1,8
      DO 80 KB=1,6
      WRT(JB,KB)=BLANK
   80 CONTINUE
C.....WRITE TITLE FOR BEST FREQUENCY TABULATION
      WRITE (LUO,6)
    6 FORMAT(//,(4X,2HUT,5X,3HBUF,5X))
C.....EACH SELECTED UT TIME
      do 5 ihr=1,nhours
      jt=ihours(ihr)
      it=jt
      if(it.gt.24) it=it-24
      FX=999.0
      FY=999.0
      XD=999.0
      YD=999.0
      FB=999.0
      IF(XLUF(IT).GE.OPMUF(IT)) GO TO 3
C.....EACH FREQUENCY
      DO 1 JF=1,11
      F=FREL(JF)
      IF(F.LE.0.01) GO TO 1
      IF(F.LT.XLUF(IT).OR.F.GT.OPMUF(IT)) GO TO 1
      X1=F
      XD1=F-FOT(IT)
      IF(XD1.LT.0.0) GO TO 2
C.....F > FOT
      IF(XD1.GT.XD) GO TO 1
      FX=X1
      XD=XD1
      GO TO 1
    2 CONTINUE
C.....F < FOT
      YD1=ABS(XD1)
      IF(YD1.GT.YD) GO TO 1
      FY=X1
      YD=YD1
    1 CONTINUE
      FB=FX
      IF(YD.LT.XD) FB=FY
      IF(FX.EQ.999.0) FB=FY
    3 CONTINUE
C ----------------------------------------------------------------------
C.....STORE B.U.F. TABLES
      J1=(IT-1)/6
      J=J1*2+1
      K=IT-J1*6
      K=(K-1)/ITSTEP+1
      I2=MOD(IT,10)
      I1=IT/10
      IF(FB.NE.999.0) GO TO 60
      WRITE(OUTX,100) I1,I2
  100 FORMAT (2I1,14X)
      GO TO 90
   60 WRITE(OUTX,110) I1,I2,FB
  110 FORMAT (2I1,F8.1,6X)
   90 READ(OUTX,120) WRT(J,K),WRT(J+1,K)
  120 FORMAT(2A8)
    5 CONTINUE
C ----------------------------------------------------------------------
C.....PRINT TABULATIONS:TIME AND BUF
      WRITE (LUO,8) ((WRT(J1,K1),J1=1,8),K1=1,6)
    8 FORMAT (/(1X,4(3X,2A8)/))
      RETURN
      END
