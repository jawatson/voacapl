c###fnorml.for
      FUNCTION FNORML(YPX)
C--------------------------------
C
C     THIS FUNCTION COMPUTES THE CUMULATIVE NORMAL DISTRIBUTION FUNCTION
C     FOR POSITIVE OR NEGATIVE VALUES OF X. SEE "THE HANDBOOK OF
C     MATHEMATICAL FUNCTIONS", AMS 55.  NOTE THAT THIS METHOD WILL WORK
C     ON 16 BIT MACHINES (32 BIT FLOATING POINT WORD)
C
      DIMENSION C(4)
      DATA C/.196854, .115194, .000344, .019527/
C
      YP = YPX
      IF(YPX) 105, 110, 110
  105 YP = - YPX
  110 YP = AMIN1(5.0, YP)
      QX = 1. + YP * (C(1) + YP * (C(2) + YP* (C(3) + YP * C(4))))
      QX = QX * QX * QX * QX
      QX = 1. / QX
      QX = .5 * QX
      IF(YPX) 115, 120, 120
  115 PX = QX
      QX = 1. - PX
      GO TO 125
  120 PX = 1. - QX
  125 CONTINUE
      FNORML = PX
      RETURN
      END
C--------------------------------
C--------------------------------
