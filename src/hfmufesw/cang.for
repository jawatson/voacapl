C FUNCTION CANG               (NEXT)
      FUNCTION CANG (Z) 
C 
C 
C      FOR USE WITH HFMUFES3 IN SYSTEMS WHERE FUNCTION CANG IS MISSING. 
C 
C 
C 
C     CANG - COMPUTES CANG=ARG(Z), Z=X + I*Y=COMPLEX NUMBER, SUCH THAT
C        -PI .LT. CANG .LE. +PI.  SEE CONTROL DATA CORP. 3600/3800
C        COMPUTER SYSTEMS LIBRARY FUNCTIONS MANUAL, PUBLICATION NO. 
C        60056400, JULY 1, 1966.
C 
      COMPLEX Z 
C 
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL 
C 
C 
C.....INITIALIZE CANG FOR QUADRANT CORRECTION, GET RE(Z) AND IM(Z), TEST
C.....X FOR + OR - TO FIND CORRECT HALF-PLANE...........................
      CANG = 0.0
      X = REAL (Z)
      Y = AIMAG (Z) 
      IF (X)100, 120, 140 
C.....X .LT. 0.0........................................................
 100  IF (Y)105, 110, 115 
 105  CANG = - PI 
      GO TO 140 
 110  CANG = + PI 
      RETURN
 115  CANG = + PI 
      GO TO 140 
C.....X .EQ. 0.0........................................................
 120  IF (Y)125, 130, 135 
 125  CANG = - PIO2 
      RETURN
C. . .X=0 AND Y=0 IS REALLY UNDEFINED, BUT IN AGREEMENT WITH CDC WE. . .
C. . .RETURN A VALUE OF CANG=0.0 . . . . . . . . . . . . . . . . . . . .
 130  CANG = 0.0
      RETURN
 135  CANG = + PIO2 
      RETURN
C.....X .GT. 0.0 AND X .6T. 0.0 ADJUSTED FOR CORRECT QUADRANT...........
 140  CANG = ATAN (Y / X) + CANG
      RETURN
      END 
