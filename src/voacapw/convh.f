c###convh.for
      SUBROUTINE  CONVH( GD,PHE,DEL,HP,RAY,CH,PTOT )
C--------------------------------
C
C     THIS SUBROUTINE CALCULATES THE GEOMETRICAL CONVERGENCE FACTOR
C
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      DENOM = AMAX1(ABS(SIN(GD)) , .000001)
      PSI   = PIO2 - DEL - PHE
      GM = GD - 2.*PSI
      GM = AMAX1(GM,0.001)
C.....GROUP PATH
      PTOT = 2.*RAY +(RZ+HP)*GM
      SMALLC = (PTOT/RZ)*COS(DEL)/DENOM
      CH  = 10.*ALOG10(SMALLC)
C.....AT ANTIPODAL
      CH  =AMIN1(CH,15.)
      RETURN
      END
C--------------------------------
