C FUNCTION REACT              (NEXT)
      FUNCTION REACT (S)
C 
C     REACT - PARAMETERS FOR THE MUTUAL REACTANCE EVALUATION BY 
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
      CR = COS (PI2 * R) / R
      FACX = 2.0 * CFAC * CR
      CR1 = COS (PI2 * R1) / R1 
      CR2 = COS (PI2 * R2) / R2 
      REACT = (((CR1 * CA1 + CR2 * CA2 - FACX * CA) * SY) / TERM + (FACX
     1 - CR1 - CR2) * SZ) * SIN (PI2 * (H2 - ABS (S))) / S
      RETURN 
      END
