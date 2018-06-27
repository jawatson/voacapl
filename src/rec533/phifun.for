      SUBROUTINE PHIFUN(T,PHI)
C
C      CALCULATION OF  0.34 * PHIEN
C
      IF(T.LE.1.0) THEN
        IF (T.LT.0.) THEN
           PHI=0.0
         ELSE
           X=(T-0.475)/0.475
           PHI=(((((-.093*X+.04)*X+.127)*X-.027)*X+.044)*X+.159)*X+.225
           PHI= AMIN1(PHI,.53)
         ENDIF
       ELSE
        IF (T.LE.2.2) THEN
          X=(T-1.65)/.55
          PHI=(((((.043*X-.07)*X-.027)*X+.034)*X+.054)*X-.049)*X+.375
          PHI= AMIN1(PHI,.53)
         ELSE
          IF (T.LE.10.0) THEN
            X=T
            PHI=.34+(((10.-X)*.02)/7.8)
          ELSE
            PHI=.34
          ENDIF
         ENDIF
        ENDIF
      RETURN
      END
