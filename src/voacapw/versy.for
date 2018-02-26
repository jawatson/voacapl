c###versy.for
      SUBROUTINE VERSY(KIK, LIK, II)
C--------------------------------
C
C     THIS ROUTINE EVALUATES THE GEOGRAPHIC VARIATION OF THE LONG TERM
C     PREDICTION MAPS. (THE MAPS WERE READ IN SUBROUTINE REDMAP)
C     THE TIME VARIATION IS PERFORMED IN SUBROUTINE VIRTIM
C     NOTE THAT "AB" IS A TWO DIMENSIONAL ARRAY TREATED AS A
C     SINGLE DIMENSIONED ARRAY
C
C KIK IS START,1,2,3 ARE ES, 4,5,F2 AND 6 IS REGULAR E.
C LIK IS STOP
C
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON /GEOG /GYZ (5), RAT (5), GMDIP (5), CLCK (5), ABIY (5), ART
     1IC (5), SIGPAT (5), EPSPAT (5)
      COMMON /A11 /GAMMA (6)
      COMMON / ONE /   IA(6), IB(6), IKIM(10,6), ESLCOF(5,55),
     1                 ESMCOF(7,61), ESUCOF(5,55), F2COF(13,76),
     2                 FM3COF(9,49), ERCOF(9,22)
      COMMON / TWO / F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     A FAM(14,12), SYS(9,16,6), PERR(9,4,6)
      COMMON /TWO_AB/ AB(318)
      DIMENSION IC (6), G (76)
      real*8 sx,x,g,cx,t    !  to solve floating point underflows
C.....IC ARE THE ARRAY LIMITS FOR EACH VARIABLE
      DATA IC/1,56,117,172,248,297/
      C360 = 360. * D2R
C.....SAMPLE AREA LONGITUDE AND LATITUDE
      CENLG = CLONG (II)
      CENLAT = CLAT (II)
      IF (CENLG)100, 105, 105
C.....CHANGE TO EAST LONGITUDE
  100 CLG = C360 + CENLG
      GO TO 110
  105 CLG = CENLG
  110 GOB = COS (CENLAT)
      DO 200 IZ = KIK, LIK
      IF (IZ - 6)115, 120, 115
C.....RAWER MAGNETIC DIP ANGLE
  115 X = GMDIP (II)
      GO TO 125
C.....USE LATITUDE FOR REGULAR E
  120 X = CENLAT
  125 Y = CLG
C.....LIMITS FOR FOURIER SERIES
      I = IKIM (9, IZ) + 1
      K = IKIM (1, IZ)
      SX = SIN (X)
      G (1) = 1.
C.....CALCULATE THE GEOGRAPHIC COORDINATE FUNCTIONS  G
      G (2) = SX
      IF (K - 1)130, 140, 130
  130 IF (K .LT. 2) GO TO 210
      DO 135 KA = 2, K
  135 G (KA + 1) = SX * G (KA)
  210 CONTINUE
  140 KDIF = IKIM (2, IZ) - K
      IF (KDIF)145, 190, 145
  145 JG = 1
      CX = GOB
      T = Y
  150 KK = IKIM (JG, IZ) + 4
      G (KK - 2) = CX * COS (T)
      G (KK - 1) = CX * SIN (T)
      LO = IKIM (JG + 1, IZ)
      IF (KDIF - 2)155, 175, 155
  155 IF (LO - KK)175, 160, 160
  160 IF (LO .LT. KK) GO TO 215
      DO 170 KA = KK, LO, 2
      IF (KA - LO)165, 165, 170
  165 G (KA) = SX * G (KA - 2)
  170 G (KA + 1) = SX * G (KA - 1)
  215 CONTINUE
  175 IF (JG - 8)180, 190, 180
  180 KDIF=IKIM(JG+2,IZ)-LO
      IF (KDIF)185, 190, 185
  185 CX = CX * GOB
      JG = JG + 1
      FJ = JG
      T = FJ * Y
      GO TO 150
  190 ISUBA = IC (IZ)
C     AB(1,IZ)
C.....FINAL SUMMATION OF THE FOURIER EXPANSION
      GAMMA (IZ) = G (1) * AB (ISUBA)
      IF (I .LT. 2) GO TO 220
      DO 195 JB = 2, I
C     AB(JB,IZ)
      ISUBA = IC (IZ) + JB - 1
      GAMMA (IZ) = GAMMA (IZ) + AB (ISUBA) * G (JB)
  195 CONTINUE
  220 CONTINUE
  200 CONTINUE
      RETURN
      END
C--------------------------------
