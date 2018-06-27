c###bear.for
      SUBROUTINE BEAR(BTR,X1,Y1,X2,Y2,GCD)
C     COMPUTE AZIMUTHS
      DIMENSION BTR(2)
      COMMON/CON/ D2R,DCL,GAMA,PI,PID,PI2,R2D,RO,VOFL
      U=.5*(PI-X1-X2+GCD)
      U1=U-PI2
      SU=SIN(U-GCD)
      XX1=X2
      XX2=X1
      YY1=Y2
      YY2=Y1
      DO 7  I=1,2
      DF=YY2-YY1
      IF(ABS(DF).LT.PI) GOTO 2
      IF(DF.GE.0.) GOTO 1
      DF=DF+PID
      GOTO 2
1     DF=DF-PID
2     SIND=SIN(U)*SIN(U1+XX1)
      IF(SIND.LE.0.) GOTO 4
      BT=114.591559*ATAN(SQRT(ABS(SIN(U1+XX2)*SU/SIND)))
      IF(DF) 6,3,5
3     IF(XX1-XX2.GE.0.) GOTO 6
4     BT=180.
      GO TO 6
5     BT=360.-BT
6     BTR(I)=BT
      XX1=X1
      XX2=X2
      YY1=Y1
7     YY2=Y2
      RETURN
      END
