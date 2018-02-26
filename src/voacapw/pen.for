c###pen.for
      FUNCTION PEN(I,K,F)
C--------------------------------
C
C     THIS ROUTINE CALCULATES THE RETARDATION FOR A PARABOLIC LAYER
C
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      X = F/FI(I,K)
      X = AMAX1(X,1.001)
      PEN =  YI(I,K)*ALOG( (1.+X)/(X-1.)) *X
      RETURN
      END
C--------------------------------
