C SUBROUTINE COLL             (NEXT)
      SUBROUTINE COLL (SUM) 
C 
C     COLL - CALCULATES THE MUTUAL IMPEDANCE BETWEEN COLINEAR DIPOLE
C        ELEMENTS OF EQUAL LENGTHS. 
C     SEE REFERENCES IN SUBROUTINE GAIN AND ALSO THE ARTICLE BY 
C     H.E. KING, IRE TRANS. ANT. AND PROP., JULY 1957, P 306. 
C 
      COMPLEX CSU1, CSU3, CSV2, CSZ1, SUM, ZE 
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
      COMMON /CUR /DIJ, EIL, HIJ, KODE
C 
C.......................................................................
      UZ = PI2 * 2.0 * (HIJ - EIL)
      U1 = PI2 * 2.0 * HIJ
      V2 = PI2 * 2.0 * (HIJ + 2.0 * EIL)
      U3 = PI2 * 2.0 * (HIJ + EIL)
      V4 = PI2 * 2.0 * (HIJ + 3.0 * EIL)
      CSU1 = CSZ1 (U1)
      CSV2 = CSZ1 (V2)
      CSU3 = CSZ1 (U3)
      HLOG = ALOG (HIJ / (HIJ + EIL)) 
      H2LOG = ALOG ((HIJ + 2. * EIL) / (HIJ + EIL)) 
      T = PI2 * (HIJ - EIL) 
      ZE = CMPLX (COS (T), SIN (T)) 
      SUM = ZE * (CSZ1 (UZ) - CSU1) + CONJG (ZE) * ALOG (HIJ / (HIJ - EI
     1L)) 
      T = PI2 * (HIJ + EIL) 
      ZE = CMPLX (COS (T), SIN (T)) 
      SUM = SUM + ZE * (CSU3 - CSV2) + CONJG (ZE) * H2LOG 
      SUM = SUM + ZE * ( - CSU1 + CSU3) + CONJG (ZE) * HLOG 
      T = PI2 * (HIJ + 3.0 * EIL) 
      ZE = CMPLX (COS (T), SIN (T)) 
      SUM = SUM + ZE * ( - CSV2 + CSZ1 (V4)) + CONJG (ZE) * ALOG ((HIJ +
     1 2.0 * EIL) / (HIJ + 3.0 * EIL))
      TS = 2.0 * COS (PI2 * EIL)
      T = PI2 * HIJ 
      ZE = CMPLX (COS (T), SIN (T)) 
      SUM = SUM + TS * ZE * ( - CSU1 + CSU3) + TS * CONJG (ZE) * HLOG 
      T = PI2 * (HIJ + 2.0 * EIL) 
      ZE = CMPLX (COS (T), SIN (T)) 
      SUM = SUM + TS * ZE * (CSU3 - CSV2) + TS * CONJG (ZE) * H2LOG 
      IF (KODE .GT. 0) GO TO 100
      SUM = SUM * 15.0
      GO TO 105 
 100  SUM = SUM * 30.0 / (1.0 - COS (2.0 * PI2 * EIL))
 105  RETURN
      END 
