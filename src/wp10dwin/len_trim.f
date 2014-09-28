      function len_trim(alf)     !  count # characters in alf
      CHARACTER*(*) alf
      max=len(alf)         !  get size of alf
      DO 10 I=MAX,1,-1
      IF(alf(I:I).NE.' ') GO TO 20
10    CONTINUE
20    len_trim=I
      RETURN
      END
C------------------------------------------------------------------
