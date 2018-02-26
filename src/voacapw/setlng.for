c###setlng.for
      SUBROUTINE SETLNG
C--------------------------------
C
C     IF SHORT PATH, THIS SUBROUTINE FILLS IN ALL OF THE ARRAYS
C
      COMMON /RAYS/ ANG(40), IFOB(40,30,5), NANG
      COMMON/ES/FS(3,5),HS(5)
      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),SIGPAT
     A (5),EPSPAT(5)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      IF(KFX -3) 105,100,100
  100 CONTINUE
      RETURN
  105 IF(KFX-1) 110,110,135
C.....ONE SAMPLE AREA
  110 CONTINUE
      DO 120 IS =2,5
      CLAT(IS)  = CLAT(1)
      CLONG(IS) = CLONG(1)
      GLAT(IS)  = GLAT(1)
C
C  IF DISTANCE INTERPOLATION IS USED THIS MAY CAUSE FAILURE.
C     (AS THE RD"S ARE EQUAL)
C
      RD(IS)    = RD(1)
      GYZ(IS)   = GYZ(1)
      RAT(IS)   = RAT(1)
      GMDIP(IS) = GMDIP(1)
      CLCK(IS)  = CLCK(1)
      ABIY(IS)  = ABIY(1)
      ARTIC(IS) = ARTIC(1)
      SIGPAT(IS)= SIGPAT(1)
      EPSPAT(IS)= EPSPAT(1)
      HS(IS)    = HS(1)
      DO 115 IL =1,3
      FS(IL,IS) = FS(IL,1)
      FI(IL,IS) = FI(IL,1)
      YI(IL,IS) = YI(IL,1)
      HI(IL,IS) = HI(IL,1)
  115 CONTINUE
  120 CONTINUE
      DO 130 IS = 2,3
      DO 125 IL= 1,30
      HPRIM(IL,IS) = HPRIM(IL,1)
      HTRUE(IL,IS) = HTRUE(IL,1)
      FVERT(IL,IS) = FVERT(IL,1)
      AFAC(IL,IS) = AFAC(IL,1)
      DO 124 IA = 1,40
  124 IFOB(IA,IL,IS) = IFOB(IA,IL,1)
  125 CONTINUE
  130 CONTINUE
      RETURN
C.....THE CASE OF 3 SAMPLE AREAS
  135 CONTINUE
      DO 145  IS =4,5
      CLAT(IS)=   CLAT(3)
      CLONG(IS) = CLONG(3)
      GLAT(IS)  = GLAT(3)
C  IF DISTANCE INTERPOLATION IS USED THIS MAY CAUSE FAILURE.
      RD(IS)    = RD(3)
      GYZ(IS)   = GYZ(3)
      RAT(IS)   = RAT(3)
      GMDIP(IS) = GMDIP(3)
      CLCK(IS)  = CLCK(3)
      ABIY(IS)  = ABIY(3)
      ARTIC(IS) = ARTIC(3)
      SIGPAT(IS) = SIGPAT(3)
      EPSPAT(IS)= EPSPAT(3)
      HS(IS)    = HS(3)
      DO 140 IL =1,3
      FS(IL,IS) = FS(IL,3)
      FI(IL,IS) = FI(IL,3)
      YI(IL,IS) = YI(IL,3)
      HI(IL,IS) = HI(IL,3)
  140 CONTINUE
  145 CONTINUE
      DO 150 IL = 1,30
      HPRIM(IL,3)= HPRIM(IL,2)
      HTRUE(IL,3)= HTRUE(IL,2)
      FVERT(IL,3) = FVERT(IL,2)
      AFAC(IL,3) = AFAC(IL,2)
      DO 149 IA = 1,40
  149 IFOB(IA,IL,3) = IFOB(IA,IL,2)
  150 CONTINUE
      RETURN
      END
C--------------------------------
