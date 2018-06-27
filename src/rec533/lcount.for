      function LCOUNT(LABEL,MAX)
      CHARACTER*(*) LABEL
      DO 10 I=MAX,1,-1
      IF(LABEL(I:I).NE.' ') GO TO 20
10    CONTINUE
20    LCOUNT=I
      RETURN
      END
C------------------------------------------------------------------
