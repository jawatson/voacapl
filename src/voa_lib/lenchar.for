      function lenchar(LABEL)
      CHARACTER*(*) LABEL
      max=len(label)
      DO 10 I=MAX,1,-1
      IF(LABEL(I:I).eq.char(0)) GO TO 10
      IF(LABEL(I:I).NE.' ') GO TO 20
10    CONTINUE
20    lenchar=I
      RETURN
      END
C------------------------------------------------------------------
