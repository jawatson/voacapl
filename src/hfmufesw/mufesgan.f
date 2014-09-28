C SUBROUTINE GAIN             (NEXT)
c          this is the GAIN routine for HFMUFES ITS-78 antennas
      SUBROUTINE MUFESGAN (KOP,KAS,TOAZ,YSIG,YEPS,YND,YNL,YNH,TEX,
     +                 DELTA, FREQ, RAIN, EFF)
      use error_codes
      save FLOG,C2KEL,S2KEL,ZT,RZERO,RIN
C 
C     GAIN - CALCULTES THE GAIN IN DECIBELS OF THE TRANSMITTING AND 
C        RECEIVING ANTENNAS.
C     KOP       ANTENNA 
C       1    TERMINATED RHOMBIC 
C       2*   VERTICAL 
C       3*   HORIZONTAL HALF-WAVE DIPOLE
C       4    HORIZONTAL YAGI
C       5    VERTICAL DIPOLE
C       6*   CURTAIN ARRAY WITH SCREEN
C       7    TERMINATED SLOPING VEE 
C       8*   INVERTED L 
C       9    TERMINATED SLOPING RHOMBIC 
C      10    ANTENNA GAINS ARE IN THE PREVIOUSLY STORED ARRAY 
C      11    SLOPING LONG WIRE
C      12    CONSTANT GAIN
C      13    HORIZONTAL LOG PERIODIC
C      14    ARBITRARY TILTED DIPOLE
C      15    HALF RHOMBIC 
C      16    SLOPING DOUBLE RHOMBOID
C      17*   VERTICAL WITH A RADIAL CONDUCTOR GROUND SYSTEM 
C 
C        NOTE  THE ANTENNA NUMBERS, (KOP) WITH AN ASTERISK AFTER THEM 
C        HAVE BEEN CHECKED AGAINST THE EQUATIONS IN MA AND WALTERS(1969)
C        AND WHERE NECESSARY BROUGHT INTO AGREEMENT WITH THAT REPORT
C 
C        NOTE  FOR KAS .NE. 0, THE ARRAY IS FILLED AUTOMATICALLY (AS IN 
C        METHOD=9) IN SUBROUTINE LUFFY IN ORDER TO
C        SAVE COMPUTATION TIME.  FOR KAS = 0, THEN, THIS ARRAY IS 
C        INTERPOLATED ON TO OBTAIN THE GAIN AT A GIVEN FREQ AND ANGLE 
C 
C 
C     ITR = 0 = CALCULATE GROUND REFLECTION LOSSES ONLY,
C         = 1 = TRANSMITTER ANTENNA GAIN CALCULATION
C         = 2 = RECEIVER ANTENNA GAIN CALCULATION 
C     KAS = 0 = INTERPOLATE GAIN FROM ARRAY 
C         = 1 = FIRST CALL FOR EACH FREQUENCY, SO CALCULATE THE 
C               IMPEDANCES, CURRENTS, ETC., 
C         .GT. 1 = NOT FIRST CALL OF FREQUENCY, SO SKIP CALCULATION 
C                  OF IMPEDANCES, CURRENTS, ETC., 
C     DELTA = TAKE-OFF ANGLE IN RADIANS,
C     FREQ = FREQUENCY IN MHZ., 
C     RAIN = THE GAIN IN DB RETURNED TO THE CALLING ROUTINE,
C     EFF = THE ANTENNA EFFICIENCY IN DB RETURNED TO THE CALLING ROUTINE
C     THE OTHER ANTENNA PARAMETERS ARE COMMUNICATED TO GAIN VIA 
C     PARAMETERS NTR, XFQB, XFQE, IANT, TOAZ, XQ, XP, XNH, XNL, XND,
C     AND TEX IN COMMON.
C 
C 
C     POWER GAIN OF ANTENNA AND GROUND REFLECTION LOSS. 
C 
C     SEE ITS-78, ESSA TECH REPORT ERL 104-ITS 74 (MA AND WALTERS,
C     APRIL 1969), AND ESSA TECH REPORT IER 54-ITSA 52 (MA AND WALTERS, 
C     1967) FOR THE DETAILS OF SUBROUTINES GAIN, CZS1, ONEJ, COLL,
C     ECH, ZMUT, CIN, SIM, CMPINV, MATINV, SQMULT, MUTUAL, AGAUSS,
C     REACT, RESIST.  SEE ALSO ANY PARTICULAR REFERENCES GIVEN IN EACH. 
C. . .SEE ALSO MA, M.T.(1974), THEORY AND APPLICATION OF ANTENNA ARRAYS 
C. . .WILEY-INTERSCIENCE. THE MAIN REFERENCE IS MA AND WALTERS (1969). .
C 
      dimension TEX(4)
      DIMENSION CIX (20), CIY (20), D (20), EX (4), R (5, 5), RDZ (5, 10
     1), RDZB (10), RDZE (5, 5), RDZZ (5), RP (5, 5), RPZB (5), RPZE (5)
     2, RT (5, 10), RTZB (10), RTZE (5, 5), RTZZ (5), RZB (5), RZE (5), 
     3TX (20, 20), TY (20, 20), XK (20), YI (20, 20), YR (20, 20), Z (20
     4, 20) 
      COMPLEX WK2,WKOK2,WK2OK,RV,RH,RC
C 
      COMPLEX ACSQ, AZH, AZV,  CSZ1, DELZ, DIF, EPHI, ETA, ETH
     1ETA1, ETHETA2,  HRATIO, QPAR, QPER, R, RDZ, RDZB, RDZE, R
     2DZZ, RHCP, RP, RPZB, RPZE, RPZZ, RT, RTZB, RTZE, RTZZ, RZB, RZE, R
     3ZZ, SQRD, V, Z, ZM, ZSUM1, ZSUM2, ZSUM3, ZSUM4, ZT, ZTERM1, ZTERM2
     4, ZTERM3, ZTERM4, ZTR, ZS, AF, CURNT, CTU, CTD, A1, B1
C 
C 
ccc      COMMON /AON /ARRAY (30, 91, 2), ITRP, ZANTP (3, 2), AEFF (30,2),
ccc     1 JA, JTR, J, MTR (2), JANT (20, 2), YNH (20, 2), YNL (20, 2),
ccc     2YND(20, 2),YETA (20, 2), TEY (4, 20, 2), YQ (20, 2), YP (20, 2),
ccc     3YFQB (20, 2), YFQE (20, 2) 
C 
ccc      COMMON /DON /ABI (24), ABIY (5, 24), ALATD, AMIN, AMIND, BRTD, BTR
ccc     1, BTRD, CLAT (5), CLCK (5, 24), CLONG (5), DLONG, DMP, EC (24), EM
ccc     2F (5, 24), ERTR, ESC (24), ESDL (24), ESDU (24), FC (24), FM (24),
ccc     3 FMM (5,24), FOES (5,24,3),SPDF (5,24), FOT (24), GCD, GCDKM, 
ccc     4GEC (24), GLAT (5), GMA (5,24), GY (5,24), GYR (24), HO (24), 
ccc     5HPFR (24), HY (5, 24), IANT (3, 2), IEA, IFQB, IFQE, IRCVR    , 
ccc     6 ITRAN    , LUFP, NOISE, NTR (2), PMP, PMUF (10, 24), 
ccc     7PWR, RD (5), RLAT, RLATD, RLONG, RLONGD, RSN, SIGTR, TEX (4, 3, 2)
ccc     8, TLAT, TLATD, TLONG, TLONGD, TOAZ (3, 2), XBTA (24), XETA (3, 2),
ccc     9 XFQB (3, 2), XFQE (3, 2), XLUF (24), XMUF (24), XND (3, 2), XNH (
ccc     A3,2),XNL(3,2),XP(3,2),XQ(3,2),YM(24),ESMUF(24),ESHPF(24),ESBTA(24)
ccc     B,ESMODE 
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
      COMMON /CUR /DIJ, EIL, HIJ, KODE
C 
      COMMON /MUT /CFAC, CT, H2, PROD1, RHI2, R21, X21, Y0, Z0
C 
      COMMON /WAITS /HH (48), XI (48) 
C 
      COMMON /HFMUFES_ONE /D1D (20), ELL (20), NMX, NO, ZS (20) 
ccc      CHARACTER*8 ITRAN(2),IRCVR(2),ESMODE(24)
ccc      CHARACTER*16 ZANTP
C 
C        48 POINT GAUSSIAN INTEGRATION - WEIGHTS AND ABSCISSAS
C 
C 
C            NOTE ALL EQUATION AND PAGE NUMBERS REFERRED TO BELOW ARE 
C            FROM MA AND WALTERS(1969) UNLESS EXPLICITLY NOTED
C            OTHERWISE.                (J)= SQRT(-1). 
C 
C 
C.....SET UP CONSTANTS AND PICK UP PARAMETERS...........................
      DATA (XI (J), J = 1, 48) / - 0.99877100725, - 0.99353017227, - 0. 
     198412458372, - 0.97059159255, - 0.95298770316, - 0.93138669071, - 
     20.90587913672, - 0.87657202027, - 0.84358826162, - 0.80706620403, 
     3- 0.76715903252, - 0.72403413092, - 0.67787237963, - 0.62886739678
     4, - 0.57722472608, - 0.52316097472, - 0.46690290475, - 0.408686481
     599, - 0.34875588629, - 0.28736248736, - 0.22476379039, - 0.1612223
     65607, - 0.097004699209, - 0.032380170963, 0.032380170963, 0.097004
     7699209, 0.16122235607, 0.22476379039, 0.28736248736, 0.34875588629
     8, 0.40868648199, 0.46690290475, 0.52316097472, 0.57722472608, 0.62
     9886739678, 0.67787237963, 0.72403413092, 0.76715903252, 0.80706620
     A403, 0.84358826162, 0.87657202027, 0.90587913672, 0.93138669071, 0
     B.95298770316, 0.97059159255, 0.98412458372, 0.99353017227, 0.99877
     C100725/ 
      DATA (HH (J), J = 1, 48) / 0.0031533460523, 0.007327553901, 0.011 
     1477234579, 0.015579315723, 0.019616160457, 0.023570760839, 0.02742
     26509708, 0.031167227833, 0.034777222565, 0.038241351066, 0.0415450
     382943, 0.044674560857, 0.047616658492, 0.050359035554, 0.052890189
     4485, 0.055199503700, 0.057277292100, 0.059114839698, 0.06070443916
     56, 0.062039423160, 0.063114192286, 0.063924238585, 0.064466164436,
     6 0.064737696813, 0.064737696813, 0.064466164436, 0.063924238585, 0
     7.063114192286, 0.062039423160, 0.060704439166, 0.059114839698, 0.0
     857277292100, 0.055199503700, 0.052890189485, 0.050359035554, 0.047
     9616658492, 0.044674560857, 0.041545082943, 0.038241351066, 0.03477
     A7222565, 0.031167227833, 0.027426509708, 0.023570760839, 0.0196161
     B60457, 0.015579315723, 0.011477234579, 0.0073275539013, 0.00315334
     C60523/
C 
ccc      DATA ARRAY/5460 * 0.0/,AEFF/60 * 0.0/ 
C 
      DATA SQRTWO/1.41421356237/,RINTW/18.06/,RINFR/100.34/,XINTR/.60001
     12/
C.....RINTW IS "RIN" FOR VERTICAL MONOPOLE AT H/LAMDA = .2
C.....RINFR IS "RIN" FOR VERTICAL DIPOLE AT H/LAMDA = .4
C.....XINTR = (4. - 1.) / (5. - 0.0001) 
C     CHANGE (LENGTH/DIAMETER) RATIO FOR HALF-WAVE ANTENNA FROM 1000
C     TO 4680. SEE MA (1974), TABLE 4.1, P. 254.
      floor=-30.                !  antenna gain floor value (was -10.)
      rain_min=.001
      RATIO = SQRTWO / 4680.0 
C.....DELTD is never used
      DELTD = DELTA * R2D 
      BETA = TOAZ
      SIGMA = YSIG
      ER = YEPS
      H = YNH
      EL = YNL
      PHI = YND
      DO 110 IKEX = 1, 4
 110  EX(IKEX) = TEX(IKEX) 
      if(DELTA.le.0.) then
         RAIN = floor
         EFF = 0.0 
         GO TO 630 
      end if
      IF (KOP .EQ. 12) GO TO 475
C.....CALCULATE WAVE=WAVELENGTH IN METERS AND SET EFFICIENCY=0DB
      WAVE = VOFL / FREQ
      EFF = 0.0 
C.....CALCULATE SIN AND COS OF TAKE-OFF ANGLE (=90-ZENITH ANG.) 
      Q = SIN (DELTA) 
      T = COS (DELTA) 
C.....DIF=COMPLEX DIELECTRIC CONSTANT=ER-(J)*18000*SIGMA/FREQ(MHZ)
C     SEE EQN.(11), P.6.
      DIF = CMPLX (ER, - 60.0 * SIGMA * WAVE) 
C.....ACSQ=RATIO OF WAVE NUMBER IN MEDIA TO THAT IN FREE SPACE, 
C     SEE EQN.(11), P. 6. 
      ACSQ = CSQRT (DIF - T * * 2)
C.....CALCULATE COMPLEX FRESNEL REFLECTION COEFFICIENT FOR VERTICAL 
C     (PERPENDICULAR) POLARIZATION(RV), EQN(9), P. 5. 
      QPER = (DIF * Q - ACSQ) / (DIF * Q + ACSQ)
      CV = CABS (QPER)
      PSIV = CANG (QPER)
C.....CALCULATE COMPLEX FRESNEL REFLECTION COEFFICIENT FOR HORIZONTAL 
C     (PARALLEL) POLARIZATION(RH), EQN(10), P. 5. 
      QPAR = (Q - ACSQ) / (Q + ACSQ)
      CH = CABS (QPAR)
      PSIH = CANG (QPAR)
C.....PUT PARAMETERS INTO WAVELENGTHS FOR ANTENNA GAIN CALCULATIONS.....
      EL1 = EL / WAVE 
      IF (EL .LT. 0.0) EL1 = ABS (EL) 
      FAC = PI * EL1
      FAC2 = PI2 * EL1
      FAC4 = 2.0 * FAC2 
      X = H / WAVE
      IF (H .LT. 0.0) X = ABS (H) 
      HWAVE = PI2 * X 
      HQWAVE = 2.0 * HWAVE * Q
      RHI = PHI * D2R 
      SR = SIN (RHI)
      CR = COS (RHI)
      RETA = BETA * D2R 
      SB = SIN (RETA) 
      CB = COS (RETA) 
C...ANTENNAS  1   2   3   4   5   6   7   8   9  10  11  12 13 14 15 16 17 =KOP
      GO TO(160,165,205,220,275,285,380,405,435,445,450,475,
     1 480, 560, 590, 595, 165), KOP 
C.....TERMINATED RHOMBIC ANTENNA, KOP=1.................................
 160  TSC = 1.0 - T * SR * CB 
      TCS = T * CR * SB 
      U1 = TSC - TCS
      U2 = TSC + TCS
      W1 = COS (PSIH - HQWAVE)
      W3 = COS (PSIV - HQWAVE)
      RAIN = 3.20 * (CR * SIN (FAC * U1) * SIN (FAC * U2) / (U1 * U2)) *
     1 * 2 * ((CB - SR * T) * * 2 * (CH * * 2 + 1.0 + 2.0 * CH * W1) + S
     2B * * 2 * (CV * * 2 + 1.0 - 2.0 * CV * W3) * Q * * 2) 
      EFF = - 1.7 
      GO TO 615 
C.....VERTICAL ANTENNA, KOP=2...........................................
 165  DMPIO2 = ABS (DELTA - PIO2) 
      RAIN = .00004 
      IF (DMPIO2 .LE. 0.5 * D2R)GOTO170 
C. . .CALCULATE A=A2,AS=B2,EQN.(30), P. 11
      SFAC2 = SIN (FAC2)
      CFAC2 = COS (FAC2)
      HQ = FAC2 * Q 
      A = COS (HQ) - CFAC2
      AS = SIN (HQ) - Q * SFAC2 
      IF(kas.le.1) then
C. . .CALCULATE RIN, EQN.(33), P. 12
         FLOG = ALOG (FAC2)
         C2KEL = 2.0 * CFAC2 * CFAC2 - 1.0 
         S2KEL = 2.0 * CFAC2 * SFAC2 
         ZT = CSZ1 (4.0 * FAC2)
C. . .LN(2)=0.6931471806, LN(4)=1.3862943612
         RZERO=.5*(C2KEL*(REAL(ZT)-FLOG-1.3862943612-GAMA)-
     +                      S2KEL*AIMAG(ZT))
         ZT = CSZ1 (FAC4)
         RZERO=30.*(RZERO+(1.+C2KEL)*(REAL(-ZT)+FLOG+0.6931471806+GAMA)
     +                     +S2KEL*AIMAG(ZT)) 
         RIN = RZERO 
         IF(EL1.lt..2) RIN = 400. * EL1 * EL1 * RINTW / 16.
         RZERO = RIN 
      end if
C.....FOR VERTICAL WITH GROUND SCREEN 
C.....CALCULATE REST OF FACTORS PER PP 13-15
      IF (KOP .EQ. 17) GO TO 175
C. . .CALCULATE GAIN USING EQNS. (29) AND (34), PP. 11-12 
      W3 = COS (PSIV) 
      W4 = SIN (PSIV) 
      RAIN = 30.0 * ((A * (1.0 + CV * W3) + AS * CV * W4) * * 2 + (A * CV * W4 + AS * (1.0 - CV * W3)) ** 2) / (RIN * T ** 2) 
      RAIN = AMAX1 (RAIN, .00004) 
 170  IF (EL1 .GE. 0.35) GO TO 615
      EFF = - ((((6416.702 * EL1 - 6091.33) * EL1 + 2179.89) * EL1 - 364
     1.817) * EL1 + 25.646) 
      GO TO 615 
C.....VERTICAL ANTENNA WITH A RADIAL CONDUCTOR GROUND SYSTEM, KOP=17....
 175  continue
      IF (kas .le. 1) then
C   . . .GET AA=LENGTH OF RADIALS IN WAVE LENGTHS
         AA = PHI / WAVE 
         IF (PHI .LT. 0.0) AA = ABS (PHI)
C   . . .CAYA=K*AA=WAVE NUMBER * LENGTH OF RADIALS 
         CAYA = PI2 * AA 
C   . . .ETA FROM EQN. IN TEXT FOLLOWING EQN.(37), P. 13 
         ETA=CSQRT(CMPLX(0.,8.*PI**2*FREQ*.1)/
     +       CMPLX(SIGMA,FREQ*ER*.001/18.))
         ALPHA = CANG (ETA) + PIO2 
C   . . .LITTLE R0 AND LITTLE R1, P. 15
         RZ = SQRT (AA * * 2 + EL1 * * 2)
         R1 = AA + RZ
         ZTR = CMPLX (0.0, PIO2) 
C   . . .DELTAZ1, EQN.(39), P.14 
      DELZ = (CSZ1 (2.0 * PI2 * (RZ + EL1)) + ZTR) * CMPLX (C2KEL, S2KEL
     1) + (CSZ1 (2.0 * PI2 * (RZ - EL1)) + ZTR) * CMPLX (C2KEL, - S2KEL)
     2 + (CSZ1 (2.0 * CAYA) + ZTR) * 2.0 * CFAC2 * * 2 + (CSZ1 (PI2 * R1
     3) + ZTR) * 4.0 * CFAC2 - (CSZ1 (PI2 * (R1 - EL1)) + ZTR) * 4.0 * C
     4FAC2 * CMPLX (CFAC2, - SFAC2) - (CSZ1 (PI2 * (R1 + EL1)) + ZTR) * 
     54.0 * CFAC2 * CMPLX (CFAC2, SFAC2)
         DELR1 = REAL (DELZ * ETA / (2.0 * PI2)) 
         ETA1 = REAL (ETA) 
         ETA2 = AIMAG (ETA)
C   . . .DELTAZ2, EQN.(40), P. 15
         DELR2 = 0.0 
         DP = AA / 2.0 
         QU = 240.0 * PI * * 2 / EX (2)
         CW = WAVE * 1000.0 / (EX (1) * EX (2))
         DO 180 J = 1, 48
         P = DP * (XI (J) + 1.0) 
         RQ = PI2 * SQRT (P * * 2 + EL1 * * 2) 
         PLOG = ALOG (P * CW)
         QQ = QU * P * PLOG
         EQ = ETA2 + QQ
         TA = ATAN2 (EQ, ETA1) 
 180     DELR2 = DELR2 + (COS(ALPHA-TA-2.*RQ) + CFAC2**2 *
     1   COS(ALPHA-TA-4.*PI*P)-2.*CFAC2*COS(ALPHA-TA-PI2*P-RQ))
     2   / SQRT(ETA1**2 + EQ**2) * PLOG * DP * HH (J) 
         DELR2 = -120.0 * PI * CABS (ETA) / EX (2) * DELR2
C   . . .RIN, EQN.(38), P. 14
         RIN = RZERO + DELR1 + DELR2 
      end if
C. . .HRATIO=A3+(J)*B3, EQN.(36), P. 13 
      HRATIO = CMPLX (0.0, 0.0) 
      DO 190 J = 1, 48
      XX = CAYA / 2.0 * (XI (J) + 1.0)
      TD = SQRT (XX * * 2 + FAC2 * * 2) 
      TS = ONEJ (XX * T)
 190  HRATIO = HRATIO + HH (J) * TS * CMPLX (COS (TD) - COS (XX) * CFAC2
     1, - SIN (TD) + SIN (XX) * CFAC2)
      HRATIO = 1.0 - HRATIO * CAYA * ETA * T / (120.0 * PI2 * A)
      RAIN = 0.0
      IF(ABS(REAL(HRATIO)).GT.2. .OR. ABS(AIMAG(HRATIO)).GT.1.)go to 195
C. . .GAIN FROM EQN.(41), P. 15 
      BP = ATAN2 (AS, A)
      CAYVH = 1.+CV**2 + 2.*CV*COS(PSIV-2.*BP) 
      TB = A / (T * COS (BP)) 
      RAIN = 30./RIN*TB**2*CAYVH*HRATIO*CONJG(HRATIO)
195   continue
      IF (EL1 .GE. 0.35) GO TO 615
      EFF=-((((6416.702*EL1-6091.33)*EL1+2179.89)*EL1-364.817)*EL1+
     1 25.646) 
      GO TO 615 
C.....HORIZONTAL HALFWAVE DIPOLE ANTENNA, KOP=3.........................
C. . .NOTE--THIS MODEL IS NOT VALID FOR ANTENNAS LESS THAN .15
C. . .WAVELENGHTHS FROM THE GROUND. 
205   SFAC = SIN (FAC)
      CFAC = COS (FAC)
      W1 = COS (PSIH - HQWAVE)
      W2 = SIN (PSIH - HQWAVE)
      W3 = COS (PSIV - HQWAVE)
      W4 = SIN (PSIV - HQWAVE)
      CPHI = T * SB 
      SPHI2 = 1. - CPHI ** 2
      RAIN = 0.0
      IF (SPHI2 .EQ. 0.0) GO TO 615 
      GI = (COS (FAC * CPHI) - CFAC) / SPHI2
      AA = SQRT (1. + CV * CV - 2. * CV * W3) * GI
      BB = SQRT (1. + CH * CH + 2. * CH * W1) * GI
      IF (kas .le. 1) then
C   . . .CALCULATE RIN, EQNS. (27),(26),(25), PP. 9-10 
         D1D (1) = 2.0 * HWAVE 
         D1D (2) = RATIO * FAC 
         SFAC2 = SIN (FAC2)
         CFAC2 = COS (FAC2)
         DO 210 J = 1, 2 
         TT = SQRT (D1D (J) * * 2 + FAC2 * * 2)
         UZ = TT - FAC2
         VZ = TT + FAC2
         TT = SQRT (D1D (J) * * 2 + FAC * * 2) 
         U1 = TT - FAC 
         V1 = TT + FAC 
         Z(J,1)=(CSZ1(UZ)-2.*CSZ1(U1))*CMPLX(CFAC2,-SFAC2)+(
     1   CSZ1(VZ)-2.*CSZ1(V1))*CMPLX(CFAC2,SFAC2)-2.*(CSZ1(U1)+CSZ1(V1))
     2   + 2.*CSZ1(D1D(J))*(CFAC2+2.)
 210     Z (J,1) = Z (J,1) * 60.0 / (1.0 - CFAC2)
         SQRD = CSQRT (DIF)
         CXC = REAL (Z (1,1) * ((1.0 - SQRD) / (1.0 + SQRD)))
         RIN = REAL (Z (2,1)) + CXC
      end if
C. . .GAIN= EQN.(28), P. 10 
      RAIN = (120. * (AA * AA * SB * SB * Q * Q + BB * BB * CB * CB)) / 
     1(RIN * SFAC * SFAC) 
      GO TO 615 
C.....HORIZONTAL YAGI ANTENNA, KOP=4....................................
 220  RAIN = 0.0
      IF (EL1 .LT. 0.25 .OR. EL1 .GT. 0.75) GO TO 615 
      W1 = CH * COS (PSIH - HQWAVE) 
      W2 = CH * SIN (PSIH - HQWAVE) 
      W3 = CV * COS (PSIV - HQWAVE) 
      W4 = CV * SIN (PSIV - HQWAVE) 
      N = EX (2)
      NM1 = N - 1 
      NM2 = N - 2 
      D (NM1) = EX (4) / WAVE 
      IF (EX (4) .LT. 0.0) D (NM1)  = ABS (EX (4))
      D (1) = EX (3) / WAVE 
      IF (EX (3) .LT. 0.0) D (1)  = ABS (EX (3))
      ELL (N) = PI * PHI / WAVE 
      IF (PHI .LT. 0.0) ELL (N)  = PI * ABS (PHI) 
      ELL (NM1) = FAC 
      ELL (1) = PI * EX (1) / WAVE
      IF (EX (1) .LT. 0.0) ELL (1)  = PI * ABS (EX (1)) 
      XK (1) = 0.0
      IF (N .EQ. 3) GO TO 230 
      IF (NM2 .LT. 2) GO TO 670 
      DO 225 J = 2, NM2 
      ELL (J) = ELL (1) 
 225  D (J) = D (1) 
 670  CONTINUE
 230  IF (N .LT. 2) GO TO 675 
      DO 235 J = 2, N 
 235  XK (J) = XK (J - 1) + D (J - 1) 
 675  CONTINUE
      IF (kas .GT. 1) GO TO 265 
C     CALL TO CIN ELIMINATED - CODE INCORPORATED BELOW
      DO 245 I = 1, N 
      NO = I
      NMX = I 
      DO 240 K = 1, NO
      D1D (K) = PI2 * ABS (XK (I) - XK (K)) 
      IF (I .EQ. K) D1D (K)  = ELL (I)  / 125.1579
 240  CONTINUE
      CALL SIM
      DO 245 J = 1, I 
      YR (NMX, J) = REAL (ZS (J)) 
      YR (J, NMX) = YR (NMX, J) 
      YI (NMX, J) = AIMAG (ZS (J))
      YI (J, NMX) = YI (NMX, J) 
 245  CONTINUE
      CALL CMPINV (YR, YI, TX, TY, N) 
      DO 250 I = 1, N 
      CIX (I) = TX (I, NM1) 
 250  CIY (I) = TY (I, NM1) 
      V = 1. / CMPLX (CIX (NM1), CIY (NM1)) 
      SUM1 = REAL (V) 
      TT = 4.0 * X * * 2
      DO 255 J = 1, NM1 
      TX1 = (NM1 - J) * * 2 
      D1D (J) = PI2 * SQRT (TT + TX1 * D (J) * * 2) 
 255  CONTINUE
      D1D (N) = PI2 * SQRT (TT + D (NM1) * * 2) 
      NMX = NM1 
      NO = N
      CALL SIM
      V = CMPLX (0.0, 0.0)
      DO 260 J = 1, N 
 260  V = V + CMPLX (CIX (J), CIY (J)) * ZS (J) 
      SQRD = CSQRT (DIF)
      SUM2 = REAL (V * (1. - SQRD) / (1. + SQRD) / CMPLX (CIX (NM1), CIY
     1 (NM1)))
      RIN = SUM1 + SUM2 
 265  CPSI = T * SB 
      SPSI2 = 1.0 - CPSI * * 2
      RAIN = 0.0
      IF (SPSI2 .EQ. 0.0) GO TO 615 
      ETR = 0.0 
      ETI = 0.0 
      PR = - CB * PI2 * T 
      IF (N .LT. 1) GO TO 710 
      DO 270 J = 1, N 
      CTK = COS (PR * XK (J)) 
      STK = SIN (PR * XK (J)) 
      SIS = 1.0 / SIN (ELL (J)) 
      TT = SIS * * 2 * (COS (ELL (J) * CPSI) - COS (ELL (J))) 
      ETR = ETR + TT * (CIX (J) * CTK - CIY (J) * STK)
      ETI = ETI + TT * (CIX (J) * STK + CIY (J) * CTK)
 270  CONTINUE
 710  CONTINUE
      EPMAG = CB * * 2 * ((ETR * (1.0 + W1) - ETI * W2) * * 2 + (ETI * (
     11.0 + W1) + ETR * W2) * * 2)
      ETMAG = SB * * 2 * Q * * 2 * ((ETR * (1.0 - W3) + ETI * W4) * * 2 
     1+ (ETI * (1.0 - W3) - ETR * W4) * * 2)
      RAIN = 120.0 * RIN * (ETMAG + EPMAG) / (SPSI2 * * 2)
      GO TO 615 
C.....VERTICAL DIPOLE ANTENNA, KOP=5....................................
 275  TIP = 0.5 * EL1 
ccc      IF (TIP .GT. X) GO TO 635 
      IF (TIP .GT. X) then      !  antenna not physically valid
         write(*,276) h,el,wave,freq,el1,tip,x
276      format('TIP>H/WAVE  H,EL,WAVE,FREQ,EL1,TIP,X=',7f10.4)
         rain=floor
         return
      end if
      CFAC = COS (FAC)
      SPHI2 = 1.0 - Q * * 2 
      RAIN = 0.0
      IF (SPHI2 .EQ. 0.0) GO TO 615 
      GI = (COS (FAC * Q) - CFAC) / SPHI2 
      W3 = COS (PSIV - HQWAVE)
      ETETA1 = - T * GI * (1.0 + CV * W3) 
      W4 = SIN (PSIV - HQWAVE)
      ETETA2 = - T * GI * CV * W4 
      HAC2 = HWAVE + HWAVE
      HAC4 = HAC2 + HAC2
      AZH = CSZ1 (HAC2) 
      W33 = REAL (AZH)
      W4 = - AIMAG (AZH)
      AZH = CSZ1 (HAC4) 
      W5 = REAL (AZH) 
      W6 = - AIMAG (AZH)
      RIN = 60. * ((1. + COS (HAC2)) *(GAMA + ALOG (HAC2) - W33) - 0.5 *
     1COS (HAC2) * (GAMA + ALOG (HAC4) - W5) + SIN (HAC2) * (.5 * W6 - W
     24)) 
      IF (EL1 - 0.4) 278, 279, 279
278   RIN = 800. * EL1 * EL1 * RINFR / 128. 
279   FMULT = 4. - XINTR * (SIGMA - 0.0001) 
      RIN = 128. * FMULT * RIN / RINFR
      RAIN = 120. * (ETETA1 ** 2 + ETETA2 ** 2) / RIN 
      GO TO 615 
C.....CURTAIN ANTENNA ARRAY WITH PERFECTLY CONDUCTING SCREEN, KOP=6.....
285   CONTINUE
      IF (BETA .LT. 90. .OR. BETA .GT. 270.) GO TO 286
      RAIN = 0.05 
      GO TO 615 
286   THETAZ = 90.0 
      DELTAP = 0.0
      EIL = 0.5 * EL1 
      NB = ABS (PHI)
      NBB = ABS (100.0 * (ABS (PHI) - NB)) + 0.5
C. . .THE BAYS ARE FED IN ANTI-PHASE(MOD NBB) IF PHI NON-INTEGER . . . .
      CBAY = + 1.0
      IF (NBB .NE. 0) CBAY = - 1.0
      IF (NBB .EQ. 0) NBB = 1 
      NE = ABS (EX (1)) 
      NEE = ABS (100.0 * (ABS (EX (1)) - NE)) + 0.5 
C. . .THE ELTS ARE FED IN ANTI-PHASE(MOD NEE) IF EX(1) IS NON-INTEGER. .
      CELE = + 1.0
      IF (NEE .NE. 0) CELE = - 1.0
      IF (NEE .EQ. 0) NEE = 1 
      DY = EX (2) / WAVE
      IF (EX (2) .LT. 0.0) DY = ABS (EX (2))
      DZ = EX (3) / WAVE
      IF (EX (3) .LT. 0.0) DZ = ABS (EX (3))
      DX = EX (4) / WAVE
      IF (EX (4) .LT. 0.0) DX = ABS (EX (4))
      IF (kas .GT. 1) GO TO 355 
C. . .CALCULATE ALL SELF- AND MUTUAL-IMPEDANCES 
      DIJ = 0.01767766952 * EIL 
      HIJ = EIL 
      KODE = 1
      CALL ZMUT (RZZ) 
C. . .SEE MA(1974),TABLE 4.1, P. 254, AND EQN.(4.114), P. 273 
      IF (REAL (RZZ) .LT. 3631.53) GO TO 290
      RZZ = CMPLX (3631.53, - 2356.47)
      KODE = - 1
      RIN = FLOAT (NB * NE) * 3631.53 
      GO TO 355 
 290  DIJ = 2.0 * DX
      CALL ZMUT (RPZZ)
      DO 295 I = 1, NE
      CI = I
      TT = (CI - 1.0) * DZ + X
      DIJ = 2.0 * TT
      CALL ZMUT (RDZZ (I))
      DIJ = 2.0 * SQRT (DX * * 2 + TT * * 2)
 295  CALL ZMUT (RTZZ (I))
      TS = (2.0 * DX) * * 2 
      IJEND = NE - 1
      DO 300 IJ = 1, IJEND
      CIJ = IJ
      DIJ = CIJ * DZ
      CALL ZMUT (RZB (IJ))
      DIJ = SQRT (TS + CIJ * * 2 * DZ * * 2)
 300  CALL ZMUT (RPZB (IJ)) 
      IPJEND = 2 * NE 
      DO 305 IPJ = 2, IPJEND
      CIPJ = IPJ
      TT = (CIPJ - 2.0) * DZ + 2.0 * X
      DIJ = TT
      CALL ZMUT (RDZB (IPJ))
      DIJ = SQRT (TS + TT * * 2)
 305  CALL ZMUT (RTZB (IPJ))
      MNEND = NB - 1
      DO 325 MN = 1, MNEND
      CMN = MN
      HIJ = CMN * DY - EIL
      CALL COLL (RZE (MN))
      DIJ = 2. * DX 
      CALL ECH (RPZE (MN))
      DO 310 I = 1, NE
      CI = I
      TT = 2.0 * ((CI - 1.0) * DZ + X)
      DIJ = TT
      CALL ECH (RDZE (MN, I)) 
      DIJ = SQRT (TS + TT * * 2)
 310  CALL ECH (RTZE (MN, I)) 
      DO 315 IJ = 1, IJEND
      CIJ = IJ
      DIJ = CIJ * DZ
      CALL ECH (R (MN, IJ)) 
      DIJ = SQRT (TS + CIJ * * 2 * DZ * * 2)
 315  CALL ECH (RP (MN, IJ))
      DO 320 IPJ = 2, IPJEND
      CIPJ = IPJ
      TT = (CIPJ - 2.0) * DZ + 2.0 * X
      DIJ = TT
      CALL ECH (RDZ (MN, IPJ))
      DIJ = SQRT (TS + TT * * 2)
 320  CALL ECH (RT (MN, IPJ)) 
 325  CONTINUE
C. . .SUM ALL SELF-AND MUTUAL-IMPEDANCES FOR ALL REAL-REAL AND REAL-. . 
C. . .IMAGE ELEMENT PAIRS . . . . . . . . . . . . . . . . . . . . ... . 
      ZSUM1 = CMPLX (0.0, 0.0)
      ZSUM2 = CMPLX (0.0, 0.0)
      ZSUM3 = CMPLX (0.0, 0.0)
      ZSUM4 = CMPLX (0.0, 0.0)
      DO 350 M = 1, NB
      DO 350 I = 1, NE
      DO 350 N = 1, NB
      DO 350 J = 1, NE
      IF (M .NE. N .OR. I .NE. J) GO TO 330 
      ZTERM1 = RZZ
      ZTERM2 = RPZZ 
      ZTERM3 = RDZZ (I) 
      ZTERM4 = RTZZ (I) 
      GO TO 345 
 330  IF (M .NE. N) GO TO 335 
      IJ = IABS (I - J) 
      ZTERM1 = RZB (IJ) 
      ZTERM2 = RPZB (IJ)
      ZTERM3 = RDZB (I + J) 
      ZTERM4 = RTZB (I + J) 
      GO TO 345 
 335  IF (I .NE. J) GO TO 340 
      MN = IABS (M - N) 
      ZTERM1 = RZE (MN) 
      ZTERM2 = RPZE (MN)
      ZTERM3 = RDZE (MN, I) 
      ZTERM4 = RTZE (MN, I) 
      GO TO 345 
 340  MN = IABS (M - N) 
      IJ = IABS (I - J) 
      ZTERM1 = R (MN, IJ) 
      ZTERM2 = RP (MN, IJ)
      ZTERM3 = RDZ (MN, I + J)
      ZTERM4 = RT (MN, I + J) 
 345  ZSUM1 = ZSUM1 + ZTERM1
      ZSUM2 = ZSUM2 + ZTERM2
      ZSUM3 = ZSUM3 + ZTERM3
      ZSUM4 = ZSUM4 + ZTERM4
 350  CONTINUE
C. . .CALCULATE THE INPUT IMPEDANCE RIN. . . . . . . . . . . . . . . . .
      SQRD = CSQRT (DIF)
      RHCP = (1.0 - SQRD) / (1.0 + SQRD)
      RIN = REAL (ZSUM1 - ZSUM2 + RHCP * (ZSUM3 - ZSUM4)) 
C. . .CALCULATE THE ELEMENT ARRAYING FACTOR (SIN(THETA)=SIN(90-DELTA)=
C. . .COS(DELTA)). . . . . . . . . . . . . .. . . . . . . . . . . . . . 
 355  STHETA = COS (DELTA)
      CPSI = STHETA * SB
      AZV = CMPLX (0.0, 0.0)
      AZH = CMPLX (0.0, 0.0)
      CTHETAZ = COS (THETAZ * D2R)
      SBZ = SIN (DELTAP * D2R)
      FACTOR = CELE 
      DO 360 M = 1, NE
      EM = M
      ZM = X + (EM - 1.0) * DZ
C. . .IF ANTI-PHASE, GROUP IN NEE-TUPLES,E.G.,++,--,++,ETC FOR NEE=2 . .
      IF (MOD (M, NEE) .EQ. 1 .OR. NEE .EQ. 1) FACTOR = FACTOR * CELE 
      TT = PI2 * ZM * (Q - CTHETAZ) 
      ZT = CMPLX (COS (TT), SIN (TT)) 
      TT = 2.0 * PI2 * ZM * Q 
      ZTR = CMPLX (COS (TT), - SIN (TT))
      AZV = AZV + ZT * (1. - QPER * ZTR) * FACTOR 
 360  AZH = AZH + ZT * (1. + QPAR * ZTR) * FACTOR 
C. . .CALCULATE THE BAY ARRAYING FACTOR . . . . . . . . . . . . . . . . 
      AF = CMPLX (1.0, 0.0) 
      IF (NB .LE. 1) GO TO 370
      AF = CMPLX (0.0, 0.0) 
      FACTOR = CBAY 
      DO 365 N = 1, NB
      EN = N
C. . .IF ANTI-PHASE, GROUP IN NBB-TUPLES,E.G.,++,--,++,ETC FOR NBB=2 . .
      IF (MOD (N, NBB) .EQ. 1 .OR. NBB .EQ. 1) FACTOR = FACTOR * CBAY 
      TT = PI2 * DY * (EN - 1.0)
      TS = CPSI - STHETA * SBZ
      ZT = CMPLX (COS (TT * TS), SIN (TT * TS)) 
 365  AF = AF + ZT * FACTOR 
C. . .CALCULATE REAL-IMAGE (SCREEN) ARRARYING FACTOR . . . . . . . . .
 370  TT = SIN (PI2 * DX * STHETA * CB) 
C. . .CALCULATE THE GAIN. . . . . . . . . . . . . . . . . . . . . . . . 
      ZT = (SB ** 2 * Q ** 2 * AZV * CONJG (AZV) + CB * * 2 * AZH * CONJG (AZH)) * TT ** 2 
      SPSI2 = 1.0 - CPSI ** 2
      RAIN = 0.0
      IF (SPSI2 .EQ. 0.0) GO TO 615 
      TT = (COS (PI2 * EIL * CPSI) - COS (PI2 * EIL)) / SPSI2 
      TRAIN = ZT * AF * CONJG (AF) * TT * * 2 / RIN 
      IF (KODE .LT. 0) GO TO 375
      RAIN = 480.0 * (1.0 / SIN (PI2 * EIL)) * * 2 * TRAIN
      GO TO 615 
C. . .SEE MA(1974) EQN.(4.114) P. 273 AND MA (PRIVATE COMM.) FOR. . . . 
C. . .CTU AND CTD VALUES FOR FULL WAVELENGTH CASE . . . . . . . . . . . 
 375  CTU = CMPLX ( - 0.0419290, + 0.0461374) 
      CTD = CMPLX ( - 0.0184019, + 0.0612938) 
      CURNT = SIN (PI2 * EIL) + CTU * (1.0 - COS (PI2 * EIL)) + CTD * (1
     1.0 - COS (PI * EIL))
      RAIN = 480.0 * TRAIN / (CURNT * CONJG (CURNT))
      GO TO 615 
C.....TERMINATED SLOPING VEE ANTENNA, KOP=7.............................
 380  HT = EX (1) / WAVE
      IF (EX (1) .LT. 0.0) HT = ABS (EX (1))
      DELTAP = ASIN ((HT - X) / EL1)
      CDELP = COS (DELTAP)
 385  RHI = ASIN (SR / CDELP) 
      CR = COS (RHI)
      SR = SIN (RHI)
      CD = CB * CR + SB * SR
      CS = CB * CR - SB * SR
      SS = SB * CR + CB * SR
      SD = SB * CR - CB * SR
      SCP = T * CDELP 
      CCP = Q * CDELP 
      SDELP = SIN (DELTAP)
      SSP = T * SDELP 
      CSP = Q * SDELP 
      U1 = 1.0 - (CSP + SCP * CD) 
      U2 = 1.0 - (CSP + SCP * CS) 
      U3 = 1.0 - ( - CSP + SCP * CD)
      U4 = 1.0 - ( - CSP + SCP * CS)
      CP5 = SSP + CCP * CD
      CP6 = SSP + CCP * CS
      CP7 = - SSP + CCP * CD
      CP8 = - SSP + CCP * CS
      W1 = COS (PSIH - HQWAVE)
      W2 = SIN (PSIH - HQWAVE)
      W3 = COS (PSIV - HQWAVE)
      W4 = SIN (PSIV - HQWAVE)
      FU1 = FAC2 * U1 
      V1 = SIN (FU1)
      Z1 = COS (FU1)
      FU2 = FAC2 * U2 
      V2 = SIN (FU2)
      Z2 = COS (FU2)
      FU3 = FAC2 * U3 
      V3 = SIN (FU3)
      Z3 = COS (FU3)
      FU4 = FAC2 * U4 
      V4 = SIN (FU4)
      Z4 = COS (FU4)
      IF (KOP .EQ. 9) GO TO 440 
      Z1 = Z1 - 1.0 
      Z2 = Z2 - 1.0 
      Z3 = Z3 - 1.0 
      Z4 = Z4 - 1.0 
      Y1 = U1 * SS
      Y3 = U3 * SS
      Y2 = U2 * SD
      Y4 = U4 * SD
      U12 = U1 * U2 
      U34 = U3 * U4 
      IF (U12 .EQ. 0.0) GO TO 390 
      A1 = (U2 * CP7 * Z1 - U1 * Z2 * CP8) / U12
      B1 = (U1 * V2 * CP8 - U2 * V1 * CP7) / U12
      C1 = (Y1 * Z2 - Y2 * Z1) / U12
      D1 = (Y2 * V1 - Y1 * V2) / U12
      GO TO 395 
 390  A1 = 0.0
      B1 = 0.0
      C1 = 0.0
      D1 = 0.0
      IF (U34 .EQ. 0.0) GO TO 615 
 395  IF (U34 .EQ. 0.0) GO TO 400 
      A2 = U3 * Z4 * CP6 - U4 * Z3 * CP5
      B2 = U3 * V4 * CP6 - U4 * V3 * CP5
      A1 = A1 + CV * (W3 * A2 + W4 * B2) / U34
      B1 = B1 + CV * ( - B2 * W3 + W4 * A2) / U34 
      AA2 = Y3 * Z4 - Y4 * Z3 
      BB2 = Y4 * V3 - Y3 * V4 
      C1 = C1 + CH * (W1 * AA2 - W2 * BB2) / U34
      D1 = D1 + CH * (W1 * BB2 + W2 * AA2) / U34
 400  RAIN = 0.05 * (A1 * * 2 + B1 * * 2 + CDELP * * 2 * (C1 * * 2 + D1 
     1* * 2)) 
      EFF = - 1.7 
      GO TO 615 
C.....INVERTED L ANTENNA, KOP=8.........................................
405   SPH = SB
      CPH = CB
      SB = Q
      CB = T
      XL = EL 
      XH = H
      EPS = ER
      SIG = SIGMA 
      F = FREQ
      WK = PI2 / WAVE 
      PSIG = -18000. * SIG / F
      WK2 = WK * CSQRT(CMPLX (EPS, PSIG)) 
      WKOK2 = WK / WK2
      WK2OK = WK2 / WK
      WL = WK * XL
      IF (XL .LT. 0.0) WL = PI2 * ABS (XL)
      WH = WK * XH
      IF (XH .LT. 0.0) WH = PI2 * ABS (XH)
      WLH = WL + WH 
      SWL = SIN (WL)
      CWL = COS (WL)
      SWLH = SIN (WLH)
C.....SWLH2 is never used (jw)
      SWLH2 = SWLH * SWLH 
      CWLH = COS (WLH)
      RC = CSQRT (1. - (WKOK2 * CB) ** 2) 
      RV = (SB - WKOK2 * RC) / (SB + WKOK2 * RC)
      RH = (SB - WK2OK * RC) / (SB + WK2OK * RC)
      RVAB = CABS (RV)
      RHAB = CABS (RH)
      PSIV = CANG (RV)
      PSIH = CANG (RH)
      CPSIPH = CB * SPH 
      PSIPH = ACOS (CPSIPH) 
      SPSIPH = SIN (PSIPH)
      SPSIPH2 = SPSIPH ** 2 
      WB = WH * SB
      SWB = SIN (WB)
      CWB = COS (WB)
      A4 = CWL * CWB - SB * SWL * SWB - CWLH
      B4 = SB * SWL * CWB + CWL * SWB - SB * SWLH 
      AB4 = SQRT (A4 * A4 + B4 * B4)
      IF (AB4 .NE. 0.0) GO TO 14
      BP = 0.0
      GO TO 15
14    BP = ATAN2 (B4, A4) 
15    WC = WL * CPSIPH
      SWC = SIN (WC)
      CWC = COS (WC)
      A5 = CWC - CWL
      B5 = SWC - CPSIPH * SWL 
      AB5 = SQRT (A5 * A5 + B5 * B5)
      IF (AB5 .NE. 0.0) GO TO 16
      BPP = 0.0 
      GO TO 17
16    BPP = ATAN2 (B5, A5)
17    PARV = BPP + PSIV - 2. * WH * SB
      PARH = BPP + PSIH - 2. * WH * SB
      IF (SPSIPH2 .NE. 0.0) GO TO 18
      F2 = 0.0
      F11 = 0.0
      G2 = 0.0
      G11 = 0.0
      GO TO 19
18    DAB5 = AB5 * SPH * SB / SPSIPH2 
      F11 = DAB5 * (COS (BPP) - RVAB * COS (PARV))
      G11 = DAB5 * (SIN (BPP) - RVAB * SIN (PARV))
      HAB5 = AB5 * CPH / SPSIPH2
      F2 = HAB5 * (COS (BPP) + RHAB * COS (PARH)) 
      G2 = HAB5 * (SIN (BPP) + RHAB * SIN (PARH)) 
19    IF (CB .NE. 0.0) GO TO 20 
      F12 = 0.0 
      G12 = 0.0 
      GO TO 21
20    DAB4 = AB4 / CB 
      F12 = - DAB4 * (COS (BP) + RVAB * COS (PSIV - BP))
      G12 = - DAB4 * (SIN (BP) + RVAB * SIN (PSIV - BP))
21    F1 = F11 + F12
      G1 = G11 + G12
      G = 30. * (F1 ** 2 + G1 ** 2 + F2 ** 2 + G2 ** 2) 
      W2H = 2. * WH 
      W4H = 2. * W2H
      CI2 = REAL (CSZ1 (W2H)) 
      CI4 = REAL (CSZ1 (W4H)) 
      CIN2 = GAMA + ALOG (W2H) - CI2
      CIN4 = GAMA + ALOG (W4H) - CI4
      SI2 = - AIMAG (CSZ1 (W2H))
      SI4 = - AIMAG (CSZ1 (W4H))
      CW2H = COS (W2H)
      SW2H = SIN (W2H)
      VRT = 30. * ((1 + CW2H) * CIN2 - 0.5 * CW2H * CIN4 - SW2H * (SI2 -
     1 0.5 * SI4))
      RIN = VRT 
      IF (X .GE. 0.2) GO TO 430 
      RIN = 400. * X * X * RINTW / 16.
430   FMULT = 4. - 3. * (SIGMA - .0001) /(5. - .0001) 
      RIN = 16. * FMULT * RIN / RINTW 
C. . .CALCULATE GAIN FROM ABOVE AND EQN.(4), P. 4 
      RAIN = G / RIN
      IF (X .GT. 0.20) GO TO 615
      EFF = 20.0 * ALOG10 (X * (6.335 + X * (67.95 - X * (693.0 - X * 16
     100.0))))
      GO TO 615 
C.....TERMINATED SLOPING RHOMBIC ANTENNA, KOP=9.........................
 435  HT = EX (1) / WAVE
      IF (EX (1) .LT. 0.0) HT = ABS (EX (1))
      DELTAP = ASIN ((HT - X) / (2.0 * EL1))
      CDELP = COS (DELTAP)
      GO TO 385 
 440  A1 = 1.0 + Z1 * Z2 - V1 * V2 - Z1 - Z2
      B1 = - V1 * Z2 - Z1 * V2 + V1 + V2
      A2 = 1.0 + Z3 * Z4 - V3 * V4 - Z3 - Z4
      B2 = - V3 * Z4 - Z3 * V4 + V3 + V4
      CM = CP8 / U2 - CP7 / U1
      CN = (CP5 / U3 - CP6 / U4) * CV 
      CMP = SD / U1 - SS / U2 
      CNP = (SD / U3 - SS / U4) * CH
      AM = CM * A1 + CN * (A2 * W3 - B2 * W4) 
      AN = CM * B1 + CN * (A2 * W4 + B2 * W3) 
      PAM = CMP * A1 + CNP * (A2 * W1 - B2 * W2)
C.....PAN is never used
      PAN = CMP * B1 + CNP * (A2 * W2 + B2 * W1)
      RAIN = 0.05 * (AM * * 2 + AN * * 2 + CDELP * * 2 * (PAM * * 2 + PA
     1N * * 2)) 
      EFF = - 1.7 
      GO TO 615 
C.....ANTENNA ALREADY IN THE ARRAY, KOP=10..............................
 445  CONTINUE
      write(*,'('' Antenna type 10 not used for HFMUFES GAIN.'')') 
      write(*,'('' MUFESGAN:445'')')
      call exit(EC_EXEC_ERROR) ! Exit if we can't find the right file.
C.....SLOPING LONG WIRE, KOP = 11.......................................
 450  CFAC2 = COS (FAC2)
      SFAC2 = SIN (FAC2)
      W1 = COS (PSIH - HQWAVE)
      W2 = SIN (PSIH - HQWAVE)
      W3 = COS (PSIV - HQWAVE)
      W4 = SIN (PSIV - HQWAVE)
      CRB = CR * CB * Q 
      SRT = SR * T
      CBS = CR * SB 
      SRQ = Q * SR
      CPHI = SRQ + T * CR * CB
      SPHI2 = 1.0 - CPHI * * 2
      CPHIP = - SRQ + T * CR * CB 
      SPHIP2 = 1.0 - CPHIP * * 2
      RAIN = 0.0
      IF (SPHI2 .EQ. 0.0 .AND. SPHIP2 .EQ. 0.0) GO TO 615 
      IF (SPHI2 .EQ. 0.0) GO TO 455 
      CIG = (COS (FAC2 * CPHI) - CFAC2) / SPHI2 
      EPHI1 = - CBS * CIG 
      ETHET1 = (CRB - SRT) * CIG
      SIG = (SIN (FAC2 * CPHI) - CPHI * SFAC2) / SPHI2
      ETHET2 = (CRB - SRT) * SIG
      EPHI2 = - CBS * SIG 
      GO TO 460 
 455  ETHET1 = 0.0
      ETHET2 = 0.0
      EPHI1 = 0.0 
      EPHI2 = 0.0 
 460  IF (SPHIP2 .EQ. 0.0) GO TO 465
      CIGP = (COS (FAC2 * CPHIP) - CFAC2) / SPHIP2
      SIGP = (SIN (FAC2 * CPHIP) - CPHIP * SFAC2) / SPHIP2
      ETHET1 = ETHET1 - (CRB + SRT) * CV * (W3 * CIGP - W4 * SIGP)
      ETHET2 = ETHET2 - (CRB + SRT) * CV * (W4 * CIGP + W3 * SIGP)
      EPHI1 = EPHI1 - CBS * CH * (W1 * CIGP - W2 * SIGP)
      EPHI2 = EPHI2 - CBS * CH * (W2 * CIGP + W1 * SIGP)
 465  continue
      IF (kas .le. 1) then
         AZH = CSZ1 (2.0 * FAC4) 
         W5 = REAL (AZH) 
         W6 = AIMAG (AZH)
         AZH = CSZ1 (FAC4) 
         W33 = REAL (AZH)
         W4 = AIMAG (AZH)
         FLOG = ALOG (FAC2) + GAMA 
         RIN=30.*(0.5*(FLOG-W5)+0.6931471806 + CFAC2 * (CFAC2 * 
     1   (FLOG - 2.0 * W33 + W5) - SFAC2 * (W6 - 2.0 * W4))) 
      end if
      RAIN=30.*(ETHET1**2 + ETHET2**2 + EPHI1**2 + EPHI2**2)/RIN
      GO TO 615 
C.....CONSTANT GAIN KOP=12..............................................
 475  RAIN = H
      EFF = 0.0 
      GO TO 630 
C.....GENERAL HORIZONTAL LOG-PERIODIC ANTENNA  KOP=13...................
 480  CONTINUE
      IF (kas .GT. 1) GO TO 550 
C     CALL TO CIN ELIMINATED - CODE INCORPORATED BELOW
      YZ = 1. / EX (1)
      N = EX (4)
      ELL (N) = FAC 
      XK(N)  =  ELL(N) *(1.0 / TAN (EX(2)*D2R)   )
      NMX = N - 1 
      DO 485 II = 1, NMX
      NII = N - II
      NIP = NII + 1 
      ELL (NII) = ELL (NIP) * EX (3)
      XK (NII) = XK (NIP) * EX (3)
 485  CONTINUE
C     DO 490 J = 1, 400 
      DO 490 J = 1, 20
      DO 490 JJ = 1, 20 
      TX (J,JJ) = 0.0 
      TY (J,JJ) = 0.0 
      YR (J,JJ) = 0.0 
 490  YI (J,JJ) = 0.0 
      DO 500 I = 1, NMX 
      MID = I + 1 
      IF (I .EQ. 1) GO TO 495 
      YI(I,I)=-YZ*(1./TAN(XK(MID)-XK(I))+1./TAN(XK(I)-XK(I-1)))
 495  YI (I, MID) = - YZ / SIN (XK (MID) - XK (I))
      YI (MID, I) = YI (I, MID) 
 500  CONTINUE
      YI(1,1) = -YZ *(1.0/TAN(XK(2) - XK(1))  ) 
      COT = 1.0 / (YZ *(1.0/TAN  (ELL(N)/2.0))  ) 
      ZTS = 0.0 
      TA = ZTS * * 2 + COT * * 2
      YRN = ZTS / TA
      YI(N,N)  =  - (COT / TA + YZ *(1.0/TAN(XK(N) - XK(NMX)))   )
      CONST = SQRTWO / 177.0
      DO 520 I = 1, N 
      JEND = I
      NO = JEND 
      NMX = JEND
      D1D (JEND) = ELL (JEND) * CONST 
      JEN = JEND - 1
      DO 505 JL = 1, JEN
 505  D1D (JL) = ABS (XK (JEND) - XK (JL))
      CALL SIM
      DO 515 JK = 1, JEND 
      Z(JK,JEND)=ZS(JK)
      IF(JK.NE.JEND) Z(JEND,JK)=ZS(JK)
 515  CONTINUE
 520  CONTINUE
      DO 525 J = 1, N 
      DO 525 I = 1, N 
      DO 525 K = 1, N 
      TX (I, J) = TX (I, J) - YI (K, I) * AIMAG (Z (K, J))
      TY (I, J) = TY (I, J) + YI (K, I) * REAL (Z (K, J)) 
      IF (I .NE. N .OR. K .NE. N) GO TO 525 
      TX (N, J) = TX (N, J) + YRN * REAL (Z (N, J)) 
      TY (N, J) = TY (N, J) + YRN * AIMAG (Z (N, J))
 525  CONTINUE
      DO 530 I = 1, N 
 530  TX (I, I) = TX (I, I) + 1.0 
      CALL CMPINV (TX, TY, YR, YI, N) 
      SUM0 = 0.0
      DO 535 I = 1, N 
      CIX (I) = YR (I,1)
      CIY (I) = YI (I,1)
 535  SUM0=SUM0+YR(I,1)*REAL(Z(I,1))-YI(I,1)*AIMAG(Z(I,1))
      SUMD = 0.0
      D1D (1) = 2.0 * HWAVE 
      TH = D1D (1) * * 2
      TXS = 4.0 * HWAVE * CR
      DO 540 NJ = 2, N
      SUMD = SUMD + XK (NJ) - XK (NJ - 1) 
 540  D1D (NJ) = SQRT (TH + SUMD * * 2 + TXS * SUMD)
      NMX = 1 
      NO = N
      CALL SIM
      V = CMPLX (0.0, 0.0)
      DO 545 J = 1, N 
 545  V = V + CMPLX (YR (I,1), YI (I,1)) * ZS (I) 
      SQRD = CSQRT (DIF)
      SUM2 = REAL (V * (1.0 - SQRD) / (1.0 + SQRD)) 
      RIN = SUM0 + SUM2 
 550  CPSI = T * SB 
      SPSI2 = 1.0 - CPSI * * 2
      RAIN = 0.0
      IF (SPSI2 .EQ. 0.0) GO TO 615 
      ETR = 0.0 
      ETI = 0.0 
      EPR = 0.0 
      EPI = 0.0 
      CQ2 = CR * Q
      CBETA = CQ2 - T * CB * SR 
      CQ2 = 2.0 * CQ2 
      DO 555 J = 1, N 
      ARG5 = CQ2 * XK (J) 
      CR5 = COS (ARG5)
      SR5 = SIN (ARG5)
      CCB = COS (XK (J) * CBETA) / SIN (ELL (J))
      SCB = SIN (XK (J) * CBETA) / SIN (ELL (J))
      TT = COS (ELL (J) * CPSI) - COS (ELL (J)) 
      A = (CIX (J) * CCB - CIY (J) * SCB) * TT
      BB = (CIY (J) * CCB + CIX (J) * SCB) * TT 
      W1 = CH * COS (PSIH - HQWAVE) 
      W2 = CH * SIN (PSIH - HQWAVE) 
      W3 = CV * COS (PSIV - HQWAVE) 
      W4 = CV * SIN (PSIV - HQWAVE) 
      C = 1.0 - (W3 * CR5 + W4 * SR5) 
      DD = W4 * CR5 - W3 * SR5
      ETR = ETR + A * C + BB * DD 
      ETI = ETI + BB * C - A * DD 
      CC = 1.0 + (W1 * CR5 + W2 * SR5)
      DC = W2 * CR5 - W1 * SR5
      EPR = EPR + A * CC - BB * DC
 555  EPI = EPI + BB * CC + A * DC
      ETMAG = (ETR * * 2 + ETI * * 2) * (Q * SB / SPSI2) * * 2
      EPMAG = (EPR * * 2 + EPI * * 2) * (CB / SPSI2) * * 2
      RAIN = 120.0 * (ETMAG + EPMAG) / RIN
      GO TO 615 
C.....ARBITARY TILTED DIPOLE ANTENNA, KOP=14............................
 560  CFAC = COS (FAC)
      W1 = COS (PSIH - HQWAVE)
      W2 = SIN (PSIH - HQWAVE)
      W3 = COS (PSIV - HQWAVE)
      W4 = SIN (PSIV - HQWAVE)
      TIP = 0.5 * EL1 * SR
      IF (TIP .GT. X) GO TO 635 
      CSB = CR * SB 
      CPHI = Q * SR + T * CSB 
      SPHI2 = 1.0 - CPHI * * 2
      CPHIP = - Q * SR + T * CSB
      SPHIP2 = 1.0 - CPHIP * * 2
      IF (SPHI2 .EQ. 0.0) GO TO 565 
      GI = (COS (FAC * CPHI) - CFAC) / SPHI2
      ETHETA1 = (CSB * Q - SR * T) * GI 
      EPHI1 = CR * CB * GI
      GO TO 570 
 565  ETHETA1 = CMPLX (0.0, 0.0)
      EPHI1 = CMPLX (0.0, 0.0)
 570  IF (SPHIP2 .EQ. 0.0) GO TO 575
      DI = (COS (FAC * CPHIP) - CFAC) / SPHIP2
      ETHETA1 = ETHETA1 - ((CSB * Q + SR * T) * DI * CV * W3) 
      EPHI1 = EPHI1 + DI * CH * W1 * CR * CB
      ETHETA2 = - (CSB * Q + SR * T) * DI * CV * W4 
      EPHI2 = CR * CB * DI * CH * W2
      GO TO 580 
 575  ETHETA2 = CMPLX (0.0, 0.0)
      EPHI2 = CMPLX (0.0, 0.0)
 580  continue
      IF (kas .GT. 1) GO TO 585 
      Y0 = RATIO * EL1
      H2 = 0.5 * EL1
      RHI2 = 0.0
      Z0 = 0.0
      CALL MUTUAL 
      R11 = R21 
      Y0 = 2.0 * X * CR 
      Z0 = 2.0 * X * SR 
      RHI2 = 2.0 * RHI
      CALL MUTUAL 
      ZM = CMPLX (R21, X21) 
      IF (RHI2 .GT. PIO2) ZM = - ZM 
      SQRD = CSQRT (DIF)
      CXC = REAL (ZM * (((1.0 - SQRD) / (1.0 + SQRD)) * CR + CMPLX (0.0,
     1 1.0) * ((DIF - SQRD) / (DIF + SQRD)) * SR) * CMPLX (CR, - SR)) 
      RIN = R11 + CXC 
 585  RAIN = 120.0 * (ETHETA1 ** 2 + ETHETA2 ** 2 + EPHI1 ** 2 + EPHI2 ** 2) / RIN
      GO TO 615 
C.....HALF RHOMBIC ANTENNA, KOP=15......................................
 590  W1 = COS (PSIH) 
      W2 = SIN (PSIH) 
      W3 = COS (PSIV) 
      W4 = SIN (PSIV) 
      TT = Q * SR 
      TS = 1.0 - T * CR * CB
      TT2 = TS + TT 
      TS2 = FAC2 * TT2
      STS4 = SIN (TS2) / TT2
      CTS4 = (1.0 - COS (TS2)) / TT2
      TT1 = TS - TT 
      TS1 = FAC2 * TT1
      STS1 = SIN (TS1)
      CTS1 = COS (TS1)
      R1 = (1.0 - CTS1) / TT1 
      FI1 = STS1 / TT1
      R4 = (1.0 - CTS1) * COS (FAC4 * SR * Q) + STS1 * SIN (FAC4 * SR * 
     1Q)
      FI4 = STS1 * COS (FAC4 * SR * Q) - SIN (FAC4 * SR * Q) * (1.0 - CT
     1S1) 
      R2 = CTS4 * CTS1 + STS4 * STS1
      FI2 = CTS1 * STS4 - CTS4 * STS1 
      F4C = (FI4 * CTS1 - R4 * STS1) / TT1
      R4C = (R4 * CTS1 + FI4 * STS1) / TT1
      RB = R1 + R2 - ((CTS4 + R4C) * W3 - (STS4 + F4C) * W4) * CV 
      BI = FI1 + FI2 - ((CTS4 + R4C) * W4 + (STS4 + F4C) * W3) * CV 
      RC = - R1 + R2 + (( - CTS4 + R4C) * W3 - ( - STS4 + F4C) * W4) * C
     1V 
      CC = - FI1 + FI2 + (( - CTS4 + R4C) * W4 + ( - STS4 + F4C) * W3) *
     1 CV 
      RA = R1 + R2 + ((CTS4 + R4C) * W1 - (STS4 + F4C) * W2) * CH 
      AI = FI1 + FI2 + ((CTS4 + R4C) * W2 + (STS4 + F4C) * W1) * CH 
      EM1 = (CR * CB * Q * RB + SR * T * RC) * * 2 + (CR * CB * Q * BI +
     1 SR * T * CC) * * 2 
      ENN1 = (CR * SB * RA) * * 2 + (CR * SB * AI) * * 2
      RAIN = 0.1 * (ENN1 + EM1) 
      RIN = 300.0 
      EFF = - 1.7 
      GO TO 615 
C.....SLOPING DOUBLE RHOMBOID, KOP=16...................................
 595  EL2 = EX (3) / WAVE 
      IF (EX (3) .LT. 0.0) EL2 = ABS (EX (3)) 
      FAK = PI2 * EL2 
      HT = EX (4) / WAVE
      IF (EX (4) .LT. 0.0) HT = ABS (EX (4))
      DEL = ASIN ((HT - X) / (EL1 + EL2)) 
      W1 = CH * COS (PSIH - HQWAVE) 
      W2 = CH * SIN (PSIH - HQWAVE) 
      W3 = CV * COS (PSIV - HQWAVE) 
      W4 = CV * SIN (PSIV - HQWAVE) 
      CDEL = COS (DEL)
      SDEL = SIN (DEL)
      SX1 = SIN (EX (1) * D2R)
      BP1 = ASIN (SX1 / CDEL) 
      CP1 = COS (BP1) 
      SP1 = SIN (BP1) 
      SX2 = SIN (EX (2) * D2R)
      BP2 = ASIN (SX2 / CDEL) 
      CP2 = COS (BP2) 
      SP2 = SIN (BP2) 
      RP1M = ((CB * CP1 - SB * SP1) * CR + (SB * CP1 + CB * SP1) * SR) *
     1 CDEL 
      RP2P = ((CB * CP2 - SB * SP2) * CR - (SB * CP2 + CB * SP2) * SR) *
     1 CDEL 
      RM2M = ((CB * CP2 + SB * SP2) * CR + (SB * CP2 - CB * SP2) * SR) *
     1 CDEL 
      RM1P = ((CB * CP1 + SB * SP1) * CR - (SB * CP1 - CB * SP1) * SR) *
     1 CDEL 
      ARGL1 = FAC4 * SDEL * Q 
      SL1 = SIN (ARGL1) 
      CL1 = SQRT (1.0 - SL1 * * 2)
      W1H1 = W1 * CL1 + W2 * SL1
      W2H1 = W2 * CL1 - W1 * SL1
      W3H1 = W3 * CL1 + W4 * SL1
      W4H1 = W4 * CL1 - W3 * SL1
      ARGL2 = 2.0 * FAK * SDEL * Q
      SL2 = SIN (ARGL2) 
      CL2 = SQRT (1.0 - SL2 * * 2)
      W1H2 = W1 * CL2 + W2 * SL2
      W2H2 = W2 * CL2 - W1 * SL2
      W3H2 = W3 * CL2 + W4 * SL2
      W4H2 = W4 * CL2 - W3 * SL2
      U1 = 1.0 - (Q * SDEL + T * RP1M)
      U2 = 1.0 - (Q * SDEL + T * RP2P)
      U3 = 1.0 - (Q * SDEL + T * RM2M)
      U4 = 1.0 - (Q * SDEL + T * RM1P)
      C11 = COS (FAC2 * U1) 
      S11 = SIN (FAC2 * U1) 
      C22 = COS (FAK * U2)
      S22 = SIN (FAK * U2)
      C23 = COS (FAK * U3)
      S23 = SIN (FAK * U3)
      C14 = COS (FAC2 * U4) 
      S14 = SIN (FAC2 * U4) 
      U1G = 1.0 / (1.0 + Q * SDEL - T * RP1M) 
      U2G = 1.0 / (1.0 + Q * SDEL - T * RP2P) 
      U3G = 1.0 / (1.0 + Q * SDEL - T * RM2M) 
      U4G = 1.0 / (1.0 + Q * SDEL - T * RM1P) 
      VR1 = (1.0 - C11) / U1
      VI1 = S11 / U1
      VR2 = (1.0 - C22) / U2
      VI2 = S22 / U2
      VR3 = (1.0 - C23) / U3
      VI3 = S23 / U3
      VR4 = (1.0 - C14) / U4
      VI4 = S14 / U4
      VR1G = (W3 * (1.0 - C11) - W4 * S11) * U1G
      VR2G = (W3 * (1.0 - C22) - W4 * S22) * U2G
      VR3G = (W3 * (1.0 - C23) - W4 * S23) * U3G
      VR4G = (W3 * (1.0 - C14) - W4 * S14) * U4G
      VR5G = (W3H2 * (1.0 - C11) - W4H2 * S11) * U1G
      VR6G = (W3H1 * (1.0 - C22) - W4H1 * S22) * U2G
      VR7G = (W3H1 * (1.0 - C23) - W4H1 * S23) * U3G
      VR8G = (W3H2 * (1.0 - C14) - W4H2 * S14) * U4G
      VI1G = (W3 * S11 + W4 * (1.0 - C11)) * U1G
      VI2G = (W3 * S22 + W4 * (1.0 - C22)) * U2G
      VI3G = (W3 * S23 + W4 * (1.0 - C23)) * U3G
      VI4G = (W3 * S14 + W4 * (1.0 - C14)) * U4G
      VI5G = (W3H2 * S11 + W4H2 * (1.0 - C11)) * U1G
      VI6G = (W3H1 * S22 + W4H1 * (1.0 - C22)) * U2G
      VI7G = (W3H1 * S23 + W4H1 * (1.0 - C23)) * U3G
      VI8G = (W3H2 * S14 + W4H2 * (1.0 - C14)) * U4G
      VR1H = (W1 * (1.0 - C11) - W2 * S11) * U1G
      VR2H = (W1 * (1.0 - C22) - W2 * S22) * U2G
      VR3H = (W1 * (1.0 - C23) - W2 * S23) * U3G
      VR4H = (W1 * (1.0 - C14) - W2 * S14) * U4G
      VR5H = (W1H2 * (1.0 - C11) - W2H2 * S11) * U1G
      VR6H = (W1H1 * (1.0 - C22) - W2H1 * S22) * U2G
      VR7H = (W1H1 * (1.0 - C23) - W2H1 * S23) * U3G
      VR8H = (W1H2 * (1.0 - C14) - W2H2 * S14) * U4G
      VI1H = (W1 * S11 + W2 * (1.0 - C11)) * U1G
      VI2H = (W1 * S22 + W2 * (1.0 - C22)) * U2G
      VI3H = (W1 * S23 + W2 * (1.0 - C23)) * U3G
      VI4H = (W1 * S14 + W2 * (1.0 - C14)) * U4G
      VI5H = (W1H2 * S11 + W2H2 * (1.0 - C11)) * U1G
      VI6H = (W1H1 * S22 + W2H1 * (1.0 - C22)) * U2G
      VI7H = (W1H1 * S23 + W2H1 * (1.0 - C23)) * U3G
      VI8H = (W1H2 * S14 + W2H2 * (1.0 - C14)) * U4G
      E1R = (VR1 - VR1G) * Q * RP1M - (VR1 + VR1G) * SDEL * T 
      E1I = (VI1 - VI1G) * Q * RP1M - (VI1 + VI1G) * SDEL * T 
      E2R = (VR2 - VR2G) * Q * RP2P - (VR2 + VR2G) * SDEL * T 
      E2I = (VI2 - VI2G) * Q * RP2P - (VI2 + VI2G) * SDEL * T 
      E3R = - (VR3 - VR3G) * Q * RM2M + (VR3 + VR3G) * SDEL * T 
      E3I = - (VI3 - VI3G) * Q * RM2M + (VI3 + VI3G) * SDEL * T 
      E4R = - (VR4 - VR4G) * Q * RM1P + (VR4 + VR4G) * SDEL * T 
      E4I = - (VI4 - VI4G) * Q * RM1P + (VI4 + VI4G) * SDEL * T 
      E5R = - C23 * ((VR1 - VR5G) * Q * RP1M - (VR1 + VR5G) * SDEL * T) 
      E5R = E5R - S23 * ((VI1 - VI5G) * Q * RP1M - (VI1 + VI5G) * SDEL *
     1 T) 
      E5I = - C23 * ((VI1 - VI5G) * Q * RP1M - (VI1 + VI5G) * SDEL * T) 
      E5I = E5I + S23 * ((VR1 - VR5G) * Q * RP1M - (VR1 + VR5G) * SDEL *
     1 T) 
      E6R = - C14 * ((VR2 - VR6G) * Q * RP2P - (VR2 + VR6G) * SDEL * T) 
      E6R = E6R - S14 * ((VI2 - VI6G) * Q * RP2P - (VI2 + VI6G) * SDEL *
     1 T) 
      E6I = - C14 * ((VI2 - VI6G) * Q * RP2P - (VI2 + VI6G) * SDEL * T) 
      E6I = E6I + S14 * ((VR2 - VR6G) * Q * RP2P - (VR2 + VR6G) * SDEL *
     1 T) 
      E7R = C11 * ((VR3 - VR7G) * Q * RM2M - (VR3 + VR7G) * SDEL * T) 
      E7R = E7R + S11 * ((VI3 - VI7G) * Q * RM2M - (VI3 + VI7G) * SDEL *
     1 T) 
      E7I = C11 * ((VI3 - VI7G) * Q * RM2M - (VI3 + VI7G) * SDEL * T) 
      E7I = E7I - S11 * ((VR3 - VR7G) * Q * RM2M - (VR3 + VR7G) * SDEL *
     1 T) 
      E8R = C22 * ((VR4 - VR8G) * Q * RM1P - (VR4 + VR8G) * SDEL * T) 
      E8R = E8R + S22 * ((VI4 - VI8G) * Q * RM1P - (VI4 + VI8G) * SDEL *
     1 T) 
      E8I = C22 * ((VI4 - VI8G) * Q * RM1P - (VI4 + VI8G) * SDEL * T) 
      E8I = E8I - S22 * ((VR4 - VR8G) * Q * RM1P - (VR4 + VR8G) * SDEL *
     1 T) 
      ETHR = E1R + E2R + E3R + E4R + E5R + E6R + E7R + E8R
      ETHI = E1I + E2I + E3I + E4I + E5I + E6I + E7I + E8I
      SP1M = (SB * CP1 + CB * SP1) * CR - (CB * CP1 - SB * SP1) * SR
      SP2P = (SB * CP2 + CB * SP2) * CR + (CB * CP2 - SB * SP2) * SR
      SM2M = (SB * CP2 - CB * SP2) * CR - (CB * CP2 + SB * SP2) * SR
      SM1P = (SB * CP1 - CB * SP1) * CR + (CB * CP1 + SB * SP1) * SR
      P1R = - (VR1 + VR1H) * SP1M 
      P1I = - (VI1 + VI1H) * SP1M 
      P2R = - (VR2 + VR2H) * SP2P 
      P2I = - (VI2 + VI2H) * SP2P 
      P3R = (VR3 + VR3H) * SM2M 
      P3I = (VI3 + VI3H) * SM2M 
      P4R = (VR4 + VR4H) * SM1P 
      P4I = (VI4 + VI4H) * SM1P 
      P5R = ((VR1 + VR5H) * C23 + (VI1 + VI5H) * S23) * SP1M
      P5I = ((VI1 + VI5H) * C23 - (VR1 + VR5H) * S23) * SP1M
      P6R = ((VR2 + VR6H) * C14 + (VI2 + VI6H) * S14) * SP2P
      P6I = ((VI2 + VI6H) * C14 - (VR2 + VR6H) * S14) * SP2P
      P7R = - ((VR3 + VR7H) * C11 + (VI3 + VI7H) * S11) * SM2M
      P7I = - ((VI3 + VI7H) * C11 - (VR3 + VR7H) * S11) * SM2M
      P8R = - ((VR4 + VR8H) * C22 + (VI4 + VI8H) * S22) * SM1P
      P8I = - ((VI4 + VI8H) * C22 - (VR4 + VR8H) * S22) * SM1P
      EPHR = (P1R + P2R + P3R + P4R + P5R + P6R + P7R + P8R) * CDEL 
      EPHI = (P1I + P2I + P3I + P4I + P5I + P6I + P7I + P8I) * CDEL 
      RAIN = 0.0296 * (ETHR * ETHR + ETHI * ETHI + EPHR * EPHR + EPHI * 
     1EPHI) 
      EFF = - 1.7 
C.....CALCULATES DECIBELS...............................................
615   IF (RAIN .LE. rain_min) RAIN = rain_min
      RAIN = 10.0 * ALOG10 (RAIN) 
C. . .ADD ADDITIONAL GAIN FOR ANTENNA TYPES 2(VERTICAL), 3(1/2 WAVE 
C     DIPOLE), AND 5(VERTICAL DIPOLE). . . . . . . . . . . . . . . . . .
      IF (KOP .EQ. 2)RAIN = RAIN + H
      IF (KOP .EQ. 3 .OR. KOP .EQ. 5)RAIN = RAIN + PHI
      IF (RAIN .LT. floor)RAIN = floor
 620  RAINE = RAIN + EFF
      IF (RAINE .GE. floor) GO TO 625
      RAINE = floor
ccc      EFF = RAINE - RAIN
 625  RAIN = RAINE
C.....NORMAL EXIT.......................................................
 630  RETURN
C.....ERROR EXIT........................................................
 635  write(*,'('' TIP > X in HUMUFES GAIN calculations.'')') 
      write(*,'('' MUFESGAN:635'')')
      call exit(EC_EXEC_ERROR) ! Exit if we can't find the right file.
 
      END
