C SUBROUTINE ZMUT             (NEXT)
      SUBROUTINE ZMUT (ZSUM)
C 
C     ZMUT - CALCULATES THE SELF-IMPEDANCE AND MUTUAL-IMPEDANCE 
C        BETWEEN LINEAR PARALLEL DIPOLE ELEMENTS OF EQUAL LENGTHS.
C     SEE THE REFERENCES IN SUBROUTINE GAIN.
C 
      COMPLEX CSU1, CSUZP, CSV1, CSZ1, ZSUM 
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
      COMMON /CUR /DIJ, EIL, HIJ, KODE
C 
C.......................................................................
      D2 = DIJ * * 2
      EL2 = EIL * * 2 
      T = SQRT (D2 + 4.0 * EL2) 
      UZ = PI2 * (T - 2.0 * EIL)
      VZ = PI2 * (T + 2.0 * EIL)
      UZP = PI2 * DIJ 
      T = SQRT (D2 + EL2) 
      U1 = PI2 * (T - EIL)
      V1 = PI2 * (T + EIL)
      W1 = 2.0 * PI2 * EIL
      CW1 = COS (W1)
      SW1 = SIN (W1)
      CSU1 = CSZ1 (U1)
      CSV1 = CSZ1 (V1)
      CSUZP = CSZ1 (UZP)
      ZSUM = (CSZ1 (UZ) - 2.0 * CSU1) * CMPLX (CW1, - SW1) + (CSZ1 (VZ) 
     1- 2.0 * CSV1) * CMPLX (CW1, SW1) + 2.0 * (CSUZP - CSU1 - CSV1) + 2
     2.0 * CSUZP * (CW1 + 1.0)
      IF (KODE .GT. 0) GO TO 100
      ZSUM = ZSUM * 30.0
      GO TO 105 
 100  ZSUM = ZSUM * 60.0 / (1.0 - CW1)
 105  RETURN
      END
