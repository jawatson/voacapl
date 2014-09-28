c###flolin.for
      SUBROUTINE FLOLIN(jfmt,idecimal,VAR,LIN)
C--------------------------------
C.....THIS SUBROUTINE WRITES A VECTOR FORMATTED LINE
C     FOR A FLOATING POINT VARIABLE
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON / OUTFMT / KLINE(26)
      DIMENSION VAR(13)
      character jfmt*(*),decimal(3)*8
      CHARACTER KLINE*6
      CHARACTER NDASH*4
      data decimal/'f5.0    ','f5.1    ','f5.2    '/
      DATA IMUF/12/, NDASH/'  - '/
      IF(LIN.le.0 .or. LIN.gt.30) return
      jfmt(8:15)=decimal(idecimal+1)
      WRITE(LUO,jfmt,IOSTAT=IERRSTAT) VAR(IMUF),(VAR(I),I=1,JFREQ)
     A    ,(NDASH,J=1,JDASH),KLINE(LIN)
      RETURN
      END
C--------------------------------
