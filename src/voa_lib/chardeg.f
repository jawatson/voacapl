      SUBROUTINE CHARDEG(CARD,LATLON,DEG,IERR)
C******************************************************************
C#  SUB CHARDEG(CARD,LATLON,DEG,IERR)  CHAR string to LAT LON degrees
C          Convert a CHARACTER string to degrees of LAT or LONG.
C          Input may be of the form:
C               dddXmm'ss"   dddXmm    dddX   ddd.dddX
C               ddd.ddd     -ddd.ddd
C          where  X=N or S for latitude
C                  =E or W for longitude
C                 W longitude is negative
C                 E latitude  is negative
C******************************************************************
C          LATLON = 0 = LATITUDE
C                 = 1 = LONGITUDE
C          *      = ERROR RETURN
C******************************************************************
      DIMENSION DMAX(2) 
      CHARACTER*(*) CARD,ICH*1
      DATA DMAX/90.,360./       !  MAX VALUES ALLOWED FOR LAT/LON
      IERR=0
      DEG=0
      MIN=0
      ISEC=0
      ISIGN=1
      IDEC=0
      IDEL=0                    !  COUNTS # OF DELIMITERS
      ionce=0
      ii=0
      DO 100 I=1,20
      ICH=CARD(I:I)
      if(ionce.eq.0 .and. ich.eq.' ') go to 100  !  skip leading blanks
      ionce=1
      IF(ICH.EQ.' ' .OR. ICH.EQ.',') GO TO 150   !  END OF STRING
      IF(IDEL.EQ.3) GO TO 80    !  SOMETHING AFTER SECONDS
      IF(ICH.EQ.'-') GO TO 10   !  - SIGN
      IF(ICH.EQ.'.') GO TO 20   !  . decimal point
      IF(ICH.GE.'0' .AND. ICH.LE.'9') GO TO 30    !  0-9 number
      IDEL=IDEL+1
      IF(IDEL.EQ.1) GO TO 70    !  1st DELIMITER--CHECK DIRECTION
      GO TO 99
10    IF(ii.NE.0) IERR=-3        !  - MUST be 1st character
      ISIGN=-1
      GO TO 99
20    IF(IDEC.NE.0) IERR=-4     !  Only 1 decimal point allowed
      IF(IDEL.NE.0) IERR=-5     !  Must be before any delimiters
      IDEC=1
      GO TO 99
30    N=ICHAR(ICH)-ICHAR('0')   !  Convert to a number
      IF(IDEL-1) 40,50,60
40    IF(IDEC.NE.0) GO TO 45    !  Fractions
      DEG=DEG*10. + FLOAT(N)
      GO TO 99
45    DEG=DEG + FLOAT(N)/10.**IDEC
      IDEC=IDEC+1
      GO TO 99
50    MIN=MIN*10 + N
      IF(MIN.GE.60) IERR=-6     !  BAD minutes
      GO TO 99
60    ISEC=ISEC*10 + N
      IF(ISEC.GE.60) IERR=-7    !  BAD seconds
      GO TO 99
70    IF(LATLON.NE.0) GO TO 75  !  LONGITUDE
      IF(ICH.EQ.'n') ICH='N'
      IF(ICH.EQ.'s') ICH='S'
      IF(ICH.NE.'N' .AND. ICH.NE.'S') IERR=-10   !  BAD DIRECTION
      IF(ICH.EQ.'S') ISIGN=-ISIGN
      GO TO 99
75    IF(ICH.EQ.'e') ICH='E'
      IF(ICH.EQ.'w') ICH='W'
      IF(ICH.NE.'E' .AND. ICH.NE.'W') IERR=-11   !  BAD DIRECTION
      IF(ICH.EQ.'W') ISIGN=-ISIGN
      GO TO 99
80    IERR=-2                   !  SOMETHING AFTER SECONDS DELIMITER
99    IF(IERR.LT.0) RETURN
      ii=ii+1
100   CONTINUE
      IERR=-8          !  Can't be this long of a character string
      RETURN
150   DEG=DEG + FLOAT(MIN)/60. + FLOAT(ISEC)/3600.
      IF(DEG.GT.DMAX(LATLON+1)) IERR=-9    !  BAD VALUE
      if(LATLON.eq.1 .and. DEG.gt.180.) DEG=DEG-360.
      DEG=DEG*ISIGN
      RETURN
      END
