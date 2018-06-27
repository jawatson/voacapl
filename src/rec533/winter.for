c###winter.for
      SUBROUTINE WINTER (ELUFY,XX,MONTH,FLUNA)
C     INFLUENCE OF THE WINTER ANOMALY TO THE LUF
      DIMENSION ELUFY(24),WIN(12)
      DATA WIN/0.30,0.15,0.03,6*0.0,0.03,0.15,0.30/
      MON=MONTH
      X=XX
      IF(X.GE.0.0) GOTO 1
      MON=MON+6
      IF(MON.GT.12) MON=MON-12
      X=-X
1     WI=WIN(MON)
      W=1.0
      IF(X.LT.30.0) GOTO 3
      IF(X.GT.60.0) GOTO 2
      W=1.0+WI*((X-30.0)/30.0)
      GOTO 3
2     W=1.0+WI*((90.0-X)/30.0)
3     DO 4 I=1,24
      IF(ELUFY(I).LE.FLUNA) GOTO 4
      ELUFY(I)=ELUFY(I)*W
4     CONTINUE
      RETURN
      END
