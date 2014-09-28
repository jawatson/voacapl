C SUBROUTINE ECH               (NEXT)
      SUBROUTINE ECH(ZSUM) 
C 
C     ECH - CALCULATES THE MUTUAL IMPEDANCE BETWEEN LINEAR DIPOLE 
C        ELEMENTS IN ECHELON OF EQUAL LENGTHS.
C     SEE THE REFERENCES IN SUBROUTINES GAIN AND COLL.
C 
      COMPLEX CSU1, CSU2, CSUZP, CSV1, CSV2, CSVZP, CSZ1, ZE, ZSUM
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
      COMMON /CUR /DIJ, EIL, HIJ, KODE
C 
C.......................................................................
      D2 = DIJ ** 2
      HML = HIJ - EIL 
      HPL = HIJ + EIL 
      T = SQRT (D2 + HML ** 2) 
      UZ = PI2 * (T + HML)
      VZ = PI2 * (T - HML)
      T = SQRT (D2 + HPL ** 2) 
      UZP = PI2 * (T - HPL) 
      VZP = PI2 * (T + HPL) 
      TS = HIJ
      T = SQRT (D2 + TS ** 2)
      U1 = PI2 * (T + TS) 
      V1 = PI2 * (T - TS) 
      TS = HIJ + 2.0 * EIL
      T = SQRT (D2 + TS ** 2)
      U2 = PI2 * (T - TS) 
      V2 = PI2 * (T + TS) 
      TS = HIJ + 3.0 * EIL
      T = SQRT (D2 + TS ** 2)
      U4 = PI2 * (T - TS) 
      V4 = PI2 * (T + TS) 
      CSU1 = CSZ1 (U1)
      CSV1 = CSZ1 (V1)
      CSU2 = CSZ1 (U2)
      CSV2 = CSZ1 (V2)
      CSUZP = CSZ1 (UZP)
      CSVZP = CSZ1 (VZP)
      ZSUM = CMPLX (0.0, 0.0) 
      T = PI2 * (EIL - HIJ) 
      ZE = CMPLX (COS (T), SIN (T)) 
      ZSUM=ZSUM + CONJG(ZE)*(CSZ1(UZ) - CSU1) + ZE*(CSZ1(VZ) - CSV1)
      T = PI2 * (EIL + HIJ) 
      ZE = CMPLX (COS (T), SIN (T)) 
      ZSUM = ZSUM + CONJG (ZE) * (CSUZP - CSU2) + ZE * (CSVZP - CSV2) 
      T = PI2 * ( - EIL - HIJ)
      ZE = CMPLX (COS (T), SIN (T)) 
      ZSUM=ZSUM + CONJG(ZE)*( -CSU1 + CSVZP) + ZE*( -CSV1 + CSUZP)
      T = PI2 * (3.0 * EIL + HIJ) 
      ZE = CMPLX (COS (T), SIN (T)) 
      ZSUM=ZSUM + CONJG(ZE)*( -CSU2+CSZ1(U4)) + ZE*( -CSV2+CSZ1(V4))
      TS = 2.0 * COS (PI2 * EIL)
      T = PI2 * HIJ 
      ZE = CMPLX (COS (T), SIN (T)) 
      ZSUM=ZSUM+TS*CONJG(ZE)*(-CSV1+CSUZP)+TS*ZE*(-CSU1+CSVZP)
      T = PI2 * (2.0 * EIL + HIJ) 
      ZE = CMPLX (COS (T), SIN (T)) 
      ZSUM=ZSUM + TS*CONJG(ZE)*(CSUZP - CSU2) + TS*ZE*(CSVZP-CSV2)
      IF (KODE .GT. 0) GO TO 100
      ZSUM = ZSUM * 15.0
      GO TO 105 
 100  ZSUM = ZSUM * 30.0 / (1.0 - COS (2.0 * PI2 * EIL))
 105  RETURN
      END
