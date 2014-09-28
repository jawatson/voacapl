C FUNCTION RESIST             (NEXT)
      FUNCTION RESIST (S) 
C 
C     RESIST - PARAMETERS FOR THE MUTUAL RESISTANCE EVALUATION BY 
C        GAUSSIAN INTEGRATION.
C     CALLED BY AGAUSS (BY DIRECTION OF MUTUAL) TO EVALUATE INTEGRAND 
C     SEE REFERENCES IN SUBROUTINE GAIN 
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
      COMMON /MUT /CFAC, CT, H2, PROD1, RHI2, R21, X21, Y0, Z0
C 
C.......................................................................
      SZ = S * CT 
      SY = S * PROD1
      TERM = Y0 + SY
      RHO2 = TERM * * 2 
      CA = Z0 + SZ
      CA1 = CA + H2 
      CA2 = CA - H2 
      R = SQRT (RHO2 + CA * * 2)
      R1 = SQRT (RHO2 + CA1 * * 2)
      R2 = SQRT (RHO2 + CA2 * * 2)
      SR = SIN (PI2 * R) / R
      FACR = 2.0 * CFAC * SR
      SR1 = SIN (PI2 * R1) / R1 
      SR2 = SIN (PI2 * R2) / R2 
      RESIST = (((SR1 * CA1 + SR2 * CA2 - FACR * CA) * SY) / TERM + (FAC
     1R - SR1 - SR2) * SZ) * SIN (PI2 * (H2 - ABS (S))) / S 
      RETURN
      END
