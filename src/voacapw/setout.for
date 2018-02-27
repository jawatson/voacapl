c# setout.f
      SUBROUTINE SETOUT
C--------------------------------
C
C     THIS ROUTINE SETS THE LINES TO BE OUTPUT IN SUBROUTINE OUTTOP AND
C     SUBROUTINE OUTBOD DEPENDENT ON METHOD. THE USER MAY SPECIFY THE
C     LINES TO BE OUTPUT BY RUNNING METHOD 23 AND SPECIFYING THE LINES
C     ON "TOPLINES" AND "BOTLINES" CONTROL CARDS
C     THE ROUTINE ALSO SETS OR ZEROS ARRAYS BEFORE EXECUTING
C
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      COMMON / OUTPRT / LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
C.....SET MUFS TO -1
      DO 100 ICR=1,24
      UTIME(ICR) = -1.
      EMUF(ICR) = -1.
      F1MUF(ICR) = -1.
      F2MUF(ICR) = -1.
      ESMUF(ICR) = -1.
      ALLMUF(ICR) = -1.
      FOT(ICR) = -1.
      XLUF(ICR) = -1.
      HPF(ICR) = -1.
  100 ANGMUF(ICR) = -1.
C.....SET CONTROL PARAMETERS
      GRPTYP = 1.
      LINES = LINMAX
      NLINE = LINMAX
      DO 105 LIN = 1,15
  105 LINTOP(LIN) = -1
      DO 110 LIN = 1,26
  110 LINBOT(LIN) = -1
C.....BRANCH TO SET USER SELECTED LINES IF METHOD = 23
      IF(METHOD.eq.23) go to 515
C.....SET LINES POSITIVE TO PRINT IN OUTTOP AND OUTBOD
      NTOP = 3
      NBOD = 1
      IF(ITRUN.eq.3 .or. ITRUN.eq.4) then
         NTOP = 3
      else IF(ITRUN.eq.8) then
         NTOP = 6
      else IF(METHOD.eq.16) then
         NTOP = 7
         NBOD = 13
      else IF(METHOD.eq.17) then
         NTOP = 7
      else IF(METHOD.eq.18) then
         NTOP = 6
      else IF(METHOD.eq.19) then
         NTOP = 3
         NBOD = 5
      else IF(METHOD.eq.20) then
         NTOP = 7
         NBOD = 21
      else IF(METHOD.eq.21) then
         NTOP = 7
         NBOD = 21
      else IF(METHOD.eq.22) then
         NTOP = 7
         NBOD = 21
      else IF(METHOD.eq.23) then
      else IF(METHOD.eq.24) then
         NTOP = 6
      else IF(METHOD.eq.25) then
         NTOP = 6
         NBOD = 22
      end if
      DO 405 LIN=1,NTOP
  405 LINTOP(LIN) = 1
C.....FIX METHODS THAT DON'T PRINT CONSECUTIVE LINES IN OUTBOD
      IF(METHOD.eq.17) then
         NBOD = 6
         LINBOT(1) = 1
         LINBOT(2) = 1
         LINBOT(5) = 1
         LINBOT(7) = 1
         LINBOT(10) = 1
         LINBOT(12) = 1
         GO TO 510
      end if
      IF(METHOD.eq.18) then
         NBOD = 6
         LINBOT(1) = 1
         LINBOT(2) = 1
         LINBOT(5) = 1
         LINBOT(7) = 1
         LINBOT(10) = 1
         LINBOT(14) = 1
         GO TO 510
      end if
      IF(METHOD.eq.24) then
         NBOD = 1
         LINBOT(12) = 1
         GO TO 510
      end if
      DO 505 LIN =1,NBOD
  505 LINBOT(LIN) = 1
C.....SET LAST ELEMENT OF LINTOP AND LINBOT TO NUMBER OF LINES
  510 LINTOP(15) = NTOP
      LINBOT(26) = NBOD
ccc      GO TO 580
C.....SET USER DEFINED LINES TO OVERRIDE METHOD IF SET BY "TOPLINES"
C.....IF METHOD = 23
  515 IF(LINTYP) 550, 550, 520
  520 DO 525 LIN = 1,15
  525 LINTOP(LIN) = -1
      NTOP = 0
      DO 540 LIN = 1,14
      ITEMP = LINTP(LIN)
      IF(ITEMP) 540, 540, 530
  530 IF(ITEMP - 14) 535, 535, 540
  535 LINTOP(ITEMP) = 1
      NTOP = NTOP + 1
  540 CONTINUE
      LINTOP(15) = NTOP
C.....SET USER DEFINED LINES TO OVERRIDE METHOD IF SET BY "BOTLINES"
C.....IF METHOD = 23
  550 IF(LINBYP) 580, 580, 555
  555 DO 560 LIN = 1,26
  560 LINBOT(LIN) = -1
      NBOD = 0
      DO 575 LIN = 1,14
      ITEMP = LINBD(LIN)
      IF(ITEMP) 575, 575, 565
  565 IF(ITEMP - 25) 570, 570, 575
  570 LINBOT(ITEMP) = 1
      NBOD = NBOD + 1
  575 CONTINUE
      LINBOT(26) = NBOD
  580 CONTINUE
      RETURN
      END
C--------------------------------
