C SUBROUTINE SIM              (NEXT)
      SUBROUTINE SIM
C 
C     SIM - CALCULATES THE MUTUAL IMPEDANCE BETWEEN LINEAR PARALLEL 
C        DIPOLE ELEMENTS OF UNEQUAL LENGTHS.
C     SEE THE REFERENCES IN SUBROUTINE GAIN.
C 
      COMMON /HFMUFES_ONE /D1D (20), ELL (20), NMX, NO, ZS (20) 
C 
      COMPLEX CSZ1, ZS
C 
C.......................................................................
      IF (NO .LT. 1) GO TO 105
      DO 100 J = 1, NO
      DJJ = D1D (J) 
      DJS = DJJ * * 2 
      HS = ELL (NMX) + ELL (J)
      HD = ELL (NMX) - ELL (J)
      CW1 = COS (HS)
      CW2 = COS (HD)
      SW1 = SIN (HS)
      SW2 = SIN (HD)
      TT = SQRT (DJS + HS * * 2)
      UZ = TT - HS
      VZ = TT + HS
      TT = SQRT (DJS + HD * * 2)
      UZP = TT - HD 
      VZP = TT + HD 
      TT = SQRT (DJS + ELL (NMX) * * 2) 
      U1 = TT - ELL (NMX) 
      V1 = TT + ELL (NMX) 
      TT = SQRT (DJS + ELL (J) * * 2) 
      U2 = TT - ELL (J) 
      V2 = TT + ELL (J) 
      ZS (J) = (CSZ1 (UZ) - CSZ1 (U1) - CSZ1 (U2)) * CMPLX (CW1, - SW1) 
     1+ (CSZ1 (VZ) - CSZ1 (V1) - CSZ1 (V2)) * CMPLX (CW1, SW1) + (CSZ1 (
     2UZP) - CSZ1 (U1) - CSZ1 (V2)) * CMPLX (CW2, - SW2) + (CSZ1 (VZP) -
     3 CSZ1 (V1) - CSZ1 (U2)) * CMPLX (CW2, SW2) + 2.0 * CSZ1 (DJJ) * (C
     4W1 + CW2) 
 100  ZS (J) = ZS (J) * 60.0 / (CW2 - CW1)
 105  CONTINUE
      RETURN
      END
