      SUBROUTINE DEG2CHR(DEGS,LATLON,CHAR)
C******************************************************************
C#  SUB DEG2CHR(DEGS,LATLON,CHAR)   Degrees to CHARACTER (xxxNxx'xx")
C          Convert degrees to characters for output.
C          DEGS = input degrees (may be -180 to 180  or  0 to 360)
C          LATLON=0= latitude   format xxxNxx'xx"
C                =1= longitude  format xxxExx'xx"
c                =2= latitude   format  xx.xxN
c                =3= longitude  format xxx.xxE
C          CHAR  = CHARACTER*10 output
C******************************************************************
      CHARACTER*(*) CHAR
      CHARACTER*1 NSEW
      DEG=DEGS
      IF(DEG.GT.180.) DEG=DEG-360.
      modd=mod(latlon,2)
      IF(modd.EQ.0 .AND. DEG.GE.0.) NSEW='N'
      IF(modd.EQ.0 .AND. DEG.LT.0.) NSEW='S'
      IF(modd.NE.0 .AND. DEG.GE.0.) NSEW='E'
      IF(modd.NE.0 .AND. DEG.LT.0.) NSEW='W'
      D=ABS(DEG)
      IDEG=D
      D=(D-IDEG)*60.
      MIN=D
      ISEC=(D-MIN)*60. + .5
      IF(ISEC.LT.60) GO TO 10
      ISEC=0
      MIN=MIN+1
      IF(MIN.LT.60) GO TO 10
      MIN=MIN-60
      IDEG=IDEG+1
10    if(latlon.le.1) then
         WRITE(CHAR,11) IDEG,NSEW,MIN,ISEC
11       format(i3,a1,i2,1h',i2,1h")
      else
         WRITE(CHAR,'(f6.2,a1)') abs(DEG),NSEW
      end if
      RETURN
      END

