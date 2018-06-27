c###snprob.for
      SUBROUTINE SNPROB(SN,SN0,XU,XL,PR)
C ------------------------------------------------------
C      SUBROUTINE SNPROB:
C..... VERSION 04.JAN.88
C         CALCULATES PROBABILITY OF GIVEN SIGNAL/NOISE LEVEL
C ------------------------------------------------------
      IF(SN-SN0)10,4,4
    4 PP=130.0-80.0/(1.0+(SN-SN0)/XL)
      PP=AMIN1(PP,99.0)
      GO TO 12
   10 PP=80.0/(1.0+(SN0-SN)/XU)-30.0
      PP=AMAX1(PP,1.0)
   12 PR=PP*0.01
      RETURN
      END
