C FUNCTION ONEJ                   (NEXT)
      FUNCTION ONEJ (X) 
C 
C     ONEJ - EVALUATES THE BESSEL FUNCTION OF FIRST KIND, J1(X).
C     SEE MATH TABLES AND OTHER AIDS TO COMPUTATION, VOL. 11, NO. 58, 
C     PAGE 86.
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
C.....COMPUTE J1(X).....................................................
      R135 = PI * 3.0 / 4.0 
      IF (X .GT. 4.0) GO TO 100 
      T = X / 4.0 
      Y = T * * 2 
      ONEJ = ((((((( - 1.289769E-4 * Y + 2.2069155E-3) * Y - 2.36616773E
     1-2) * Y + 0.1777582922) * Y - 0.8888839649) * Y + 2.666666054) * Y
     2 - 3.999999971) * Y + 2.0) * T
      GO TO 105 
 100  T = 4.0 / X 
      Y = T * * 2 
      PSUM = (((((4.2414E-6 * Y - 2.0092E-5) * Y + 5.80759E-5) * Y - 2.2
     13203E-4) * Y + 2.9218256E-3) * Y + 0.3989422819) * 2.50662827 
      QSUM = ((((( - 3.6594E-6 * Y + 1.622E-5) * Y - 3.98708E-5) * Y + 1
     1.064741E-4) * Y - 6.3904E-4) * Y + 3.74008364E-2) * 2.50662827 * T
      TX = COS (X - R135) 
      TY = SIN (X - R135) 
      TS = SQRT (2.0 / (PI * X))
      ONEJ = TS * (PSUM * TX - QSUM * TY) 
 105  RETURN
      END
