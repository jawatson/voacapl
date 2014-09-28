C COMPLEX FUNCTION CSZ1       (NEXT)
      COMPLEX FUNCTION CSZ1 (X) 
C 
C     CSZ1 - EVALUATES THE COSINE-INTEGRAL AND SINE-INTEGRAL FUNCTIONS. 
C     SEE THE REFERENCES IN SUBROUTINE GAIN AND THOSE GIVEN BELOW.
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
      COMPLEX A,AM1, AM2, B,BM1, BM2, SA
      real*8 testq,t,tm1
C 
C 
C.....TEST ARGUMENT RANGE...............................................
ccc      DATA TESTQ / 4.0E-10/ 
      DATA TESTQ / 4.0E-9/     !  the above values cause floating point errors
      IF (X .GT. 6.0) GO TO 120 
C.....FOR X .LE. 6 USE THE SERIES EXPANSION.............................
C. . .SEE THE HANDBOOK OF MATHEMATICAL FUNCTIONS, ABRAMOWITZ AND STEGUN,
C. . .P 232, EQU 5.2.14 AND 5.2.16 . . . . . . . . . . . . . . . . . . .
      EN = 0.0
      X2 = X ** 2
      TN = X
      SI = X
 100  EN = EN + 1.0 
      TN = - TN * X2 * (2.0 * EN - 1.0) / ((2.0 * EN) * (2.0 * EN + 1.0)
     1 ** 2) 
      IF (ABS (TN / SI) .LE. TESTQ) GO TO 105 
      SI = SI + TN
      GO TO 100 
 105  EN = 1.0
      TN = - X2 / 4.0 
      CI = TN + GAMA + ALOG (X) 
 110  EN = EN + 1.0 
      TN = - TN * X2 * (2.0 * EN - 2.0) / ((2.0 * EN - 1.0) * (2.0 * EN)
     1 ** 2) 
      IF (ABS (TN / CI) .LE. TESTQ) GO TO 115 
      CI = CI + TN
      GO TO 110 
 115  CSZ1 = CMPLX (CI, - SI) 
      GO TO 145 
C.....FOR X .GT. 6 USE THE CONTINUED FRACTION EXPANSION.................
C. . .SEE  ANALYSIS AND DESIGN OF THE LOG PERIODIC DIPOLE ANTENNA. . . .
C. . .BY ROBERT L. CARREL, UNIV. OF ILL. ANTENNA LAB. TECH. REPORT . . .
C. . .NUMBER 52 (NTIS NO. AD264558), PAGE 177 (APPENDIX A) . . . . . . .
 120  AM1 = (1.0, 0.0)
      AM2 = (1.0, 0.0)
      BM1 = (1.0, 0.0)
      BM2 = (0.0, 0.0)
      P = 0.0 
      K = 0 
      TM1 = 0.0 
 125  P = P + 1.0 
      K = K + 1 
      IF (MOD (K, 2) .EQ. 0) GO TO 130
      SA = CMPLX (0.0, (P + 1.0) / (2.0 * X)) 
      GO TO 135 
 130  SA = CMPLX (0.0, P / (2.0 * X)) 
 135  A = AM1 + SA * AM2
      B = BM1 + SA * BM2
      CSZ1 = A / B
      T = CABS (CSZ1) 
      IF (DABS ((T - TM1) / T) .LE. TESTQ) GO TO 140 
      AM2 = AM1 
      AM1 = A 
      BM2 = BM1 
      BM1 = B 
      TM1 = T 
      GO TO 125 
140   CSZ1=CMPLX(0.0,PIO2)+CMPLX(COS(X),SIN(X))/(CMPLX(0.0,X)*CSZ1)
      CSZ1 = CONJG (CSZ1) 
 145  RETURN
      END 
