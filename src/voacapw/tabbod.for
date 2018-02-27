c###tabbod.for
      SUBROUTINE TABBOD
C--------------------------------
C.....THIS ROUTINE TABULATES A VARIABLE SET IN OUTTAB USING A VECTORED
C.....FORMAT SCHEME WHICH PRINTS A DASH IF THE VARIABLE .LE. 0 AND THE
C.....VALUE OTHERWISE
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      COMMON / OUTGH / PLOTI(24), PLOTJ(24), PLOTK(24), XOUT(13), NPLOTS
      COMMON / OUTPRT / LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      DIMENSION XFMT(13)
      save ifmt
      CHARACTER IGMT*5, ILMT*5, IMUF*5, IFMT(26)*6, NFMT(11)*6
      DATA IGMT/'  GMT '/, ILMT/'  LMT'/, IMUF/'  MUF'/
      DATA NFMT/'(     ', '  ,2X ', '3F5.1 ', ',3X,  ', '2H-   ',
     A',F5.2 ', '3A5   ', ',F5.1 ', '      ', ',F6.2)', ',A5,/)'/
      JOUT = LUO
      IF(UTIME(IT).le.0) go to 500      !  return if TIME set negative
      IF(ALLMUF(IT).le.1.) go to 500    !  return if MUF = 1.0
C.....SET THE ENTIRE LINE TO PRINT DASHES (CHANGE LATER WHERE NECESSARY)
      IFMT(1) = NFMT(1)
      DO 120 J = 4, 24, 2
  120 IFMT(J) = NFMT(4)
      DO 130 J = 5, 25, 2
  130 IFMT(J) = NFMT(5)
C.....PRINT HEADING IF OUTTOP JUST PRINTED
      IF(LINES.gt.9) go to 220
      WRITE(JOUT,141)
  141 FORMAT(/,27X,'FREQUENCY / RELIABILITY',/)
C.....PRINT FREQUENCIES ( OR DASHES IF THEY AREN"T SET)
      IFREQ = 0
      IFMT(2) = NFMT(7)
      IFMT(3) = NFMT(2)
      IFMT(26) = NFMT(11)
      IF(FREL(14).le.-2.) go to 192
      DO 170 IFQ = 1, 11
      IF(FREL(IFQ).le.0.) go to 170
         IFREQ = IFREQ + 1
         XFMT(IFREQ) = FREL(IFQ)
  170 CONTINUE
      KNT = 4
      DO 180 IFQ = 1, IFREQ
      IFMT(KNT) = NFMT(8)
      IFMT(KNT+1) = NFMT(9)
      KNT = KNT + 2
  180 CONTINUE
      IF(IFREQ.le.0) go to 195
C  TEMP FIX WHEN FOT IS INSERTED
  192 IF(FREL(14).gt.0.) go to 200
      XFMT(1) = 0.0
      GO TO 200
  195 WRITE(JOUT,IFMT) IGMT, ILMT, IMUF, IMUF
      GO TO 205
  200 WRITE(JOUT,IFMT) IGMT, ILMT, IMUF, (XFMT(IFQ),IFQ=1,IFREQ), IMUF
  205 LINES = LINES + 1
C.....RE-INITIALIZE LINE TO PRINT ALL DASHES (CHANGE WHERE NECESSARY)
      DO 210 J = 4, 24, 2
  210 IFMT(J) = NFMT(4)
      DO 215 J = 5, 25, 2
  215 IFMT(J) = NFMT(5)
C.....CHANGE FORMAT VECTORS TO XOUT INSTEAD OF FREQUENCY
  220 IFMT(2) = NFMT(3)
      IFMT(26) = NFMT(10)
C.....TABULATE THE VARIABLE (OR DASHES IF VARIABLE .LE.0)
      IFREQ = 0
      KNT = 4
      DO 250 IFQ = 1, 11
      IF(XOUT(IFQ).gt.0.) then
         IFREQ = IFREQ + 1
         XFMT(IFREQ) = XOUT(IFQ)
         IFMT(KNT) = NFMT(6)
         IFMT(KNT+1) = NFMT(9)
      end if
  250 KNT = KNT + 2
      IF(IFREQ.le.0) then
         WRITE(JOUT,IFMT) UTIME(IT), XLMT(IT), ALLMUF(IT), XOUT(12)
      else
         WRITE(JOUT,IFMT) UTIME(IT), XLMT(IT), ALLMUF(IT),
     +                    (XFMT(IFQ), IFQ = 1, IFREQ), XOUT(12)
      end if
      LINES = LINES + 1
  500 RETURN
      END
C--------------------------------
