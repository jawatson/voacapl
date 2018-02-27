      SUBROUTINE DBLTRAP(DINTGL)
C*********************************************************************
C  PARAMETERS:
C     DINTGL  -  INTEGRATION RESULT CALCULATED IN DBLTRAP
C     THETA STEP SIZE = 1  DEGREE
C     PHI   STEP SIZE = 1  DEGREE
C     THETA LOWER LIMIT OF INTEGRATION = 0. DEGREES
C     PHI   LOWER LIMIT OF INTEGRATION = -90. DEGREES
C     THETA UPPER LIMIT OF INTEGRATION = 90. DEGREES
C     PHI   UPPER LIMIT OF INTEGRATION = 90. DEGREES
C*********************************************************************
      DATA D2R/.01745329251/
      PHIZ = -90. * D2R
      PHIF =  90. * D2R
      TINT = 0.0
      T = 0.
      DO 80 IP1 = 1,91
        P = PHIZ + D2R
        PINT = 0.0
        DO 90 J = 1,179
          PINT = PINT + F2(T,P)
          P = P + D2R
   90   CONTINUE
        PINT = PINT * D2R
        PINT = PINT + (D2R/2.) * (F2(T,PHIF) + F2(T,PHIZ))
        IF (IP1.EQ.1 .OR. IP1.EQ.91) GO TO 91
        TINT = TINT + D2R * PINT *COS(T)
        GO TO 92
   91   TINT = TINT + D2R * PINT * COS(T) / 2.0
   92   CONTINUE
        T = T + D2R
   80 CONTINUE
      DINTGL =  TINT
      RETURN
      END
C------------------------------------------------------------------
