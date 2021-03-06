c###geotim.for
      SUBROUTINE GEOTIM(JT)
C--------------------------------
C
C     THIS ROUTINE CALLS FUNCTION CNGTIM TO CONVERT FROM UT TO LMT, ETC.
C
      COMMON / CON / D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / GEOG / GYZ(5), RAT(5), GMDIP(5), CLCK(5), ABIY(5),
     1 ARTIC(5), SIGPAT(5), EPSPAT(5)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      CKC = JT
C.....I!ITIM IS SPECIFIED ON THE "TIME" CONTROL CARD.
C.....IF ITIM .LT.0 USE LMT SO CALCULATE UT HERE
      IF(ITIM) 120, 130, 130
C.....LMT AT TRANSMITTER
  120 TEMP = CKC
      DUMMY = TLONG * R2D
      CNGDAY = CNGTIM(CKC,DUMMY,-1)
      GMT = CKC
      GO TO 140
C.....GMT AT TRANSMITTER
  130 GMT = CKC
      DUMMY = TLONG * R2D
      CNGDAY = CNGTIM(CKC,DUMMY,1)
      TEMP = CKC
  140 IT = GMT
      IF(IT) 150, 150, 160
  150 IT = 24
  160 UTIME(IT) = GMT
      XLMT(IT) = TEMP
C.....LMT AT RECEIVER
      CKC = GMT
      DUMMY = RLONG * R2D
      CNGDAY = CNGTIM(CKC,DUMMY,1)
      GMTR = CKC
C.....LMT AT SAMPLE POINTS
      IF(KM) 190, 190, 170
  170 DO 180 II = 1,KM
      CKC = GMT
      DUMMY = CLONG(II) * R2D
      CNGDAY = CNGTIM(CKC,DUMMY,1)
  180 CLCK(II) = CKC
  190 RETURN
      END
C--------------------------------
