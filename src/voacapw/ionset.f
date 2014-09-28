c###ionset.for
      SUBROUTINE IONSET
C--------------------------------
C
C     THIS ROUTINE CHECKS THE CONSISTANCY OF THE IONOSPHERIC PARAMETERS.
C     IT ALSO PLACES THE PARAMETERS INTO PROPER SLOTS USING THE
C     TRADITIONAL RULE (IE. TAKE THE E LAYER FROM THE CLOSEST SAMPLE
C     AREA AND THE F LAYER FROM THE FURTHER SAMPLE AREA.)
C
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      IF (KM - 1)105, 105, 110
  105 KFX = 1
      GO TO 125
  110 IF (KM - 3)115, 115, 120
  115 KFX=2
      FI(3,1)=FI(3,2)
      YI(3,1)=YI(3,2)
      HI(3,1)=HI(3,2)
      FI (1, 2) = FI (1, 3)
      YI (1, 2) = YI (1, 3)
      HI (1, 2) = HI (1, 3)
      FI(2,2)=FI(2,3)
      YI(2,2)=YI(2,3)
      HI(2,2)=HI(2,3)
      GO TO 125
  120 CONTINUE
      KFX = 3
      FI(3,1)=FI(3,2)
      YI(3,1)=YI(3,2)
      HI(3,1)=HI(3,2)
      DO 122 I=1,3
      FI(I,2)=FI(I,3)
      YI(I,2)=YI(I,3)
  122 HI(I,2)=HI(I,3)
      FI(1,3)=FI(1,5)
      YI(1,3)=YI(1,5)
      HI(1,3)=HI(1,5)
      FI(2,3)=FI(2,5)
      YI(2,3)=YI(2,5)
      HI(2,3)=HI(2,5)
      FI(3,3)=FI(3,4)
      YI(3,3)=YI(3,4)
      HI(3,3)=HI(3,4)
  125 CONTINUE
      IF (KFX .LT. 1) GO TO 165
      DO 155 K = 1, KFX
      IF (FI (2, K))150, 150, 130
C  CHECK  E-F1 CRITICALS
  130 IF (FI (2, K) - FI (1, K) - 0.2)135, 135, 140
  135 FI (2, K) = 0.0
      YI (2, K) = 0.0
      HI (2, K) = 0.0
      GO TO 150
C     CHECK  F1-F2 CRITICALS
  140 IF (FI (3, K) - FI (2, K) - 0.2)135, 135, 145
  145 HDIF = HI (2, K) - HI (1, K) + 1.
      HI(2,K) = AMIN1(HI(2,K), HI(3,K))
  150 HDIF = HI (3, K) - HI (1, K) - 2.
      YI (3, K) = AMIN1 (YI (3, K), HDIF)
  155 CONTINUE
  165 RETURN
      END
C--------------------------------
