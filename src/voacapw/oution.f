c###oution.for
      SUBROUTINE OUTION
C--------------------------------
C
C     THIS ROUTINE CALLS SUBROUTINES TO GENERATE AND OUTPUT IONOGRAMS
C
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      DO 100 K=1,KFX
      CALL LECDEN(K)
      CALL GENION(K)
      CALL IONPLT(K)
  100 CONTINUE
      RETURN
      END
C--------------------------------
