c# redmap.f
      SUBROUTINE REDMAP(SSN,MONTH)
C--------------------------------
C
C     THIS ROUTINE READS THE IONOSPHERIC LONG TERM DATA BASE FILE
c*****************************************************************
c     An error in the coefficient data file was corrected on 3.Nov.94
c     The values for SYS(9,2,6) & SYS(9,10,6) for SPRING & FALL were wrong.
c     They were 115.2 and should have been 11.52.
c     This required no programming change, but the comment was placed here
c     to record the error for posterity.  The change was corrected in the
c     ASCII data file and then it was converted to binary.
c*****************************************************************
C
      common /crun_directory/ run_directory
         character run_directory*50
      common /ccoeff/ coeff
      character coeff*4
C.....F1 LAYER MONTHLY COEFFICIENTS
      COMMON /FONE/ ANEW(3),BNEW(3),ACHI(2),BCHI(2)
C.....COEFFICIENTS FOR CRITICAL FREQUENCIES AND F2 LAYER HEIGHT OF
C.....MAXIMUM / SEMITHICKNESS RATIO
      COMMON /ONE/ IA(6),IB(6),IKIM(10,6),ESLCOF(5,55),ESMCOF(7,61),
     +             ESUCOF(5,55),F2COF(13,76),FM3COF(9,49),ERCOF(9,22)
ccc   COMMON / RAYS / ANG(40), IFOB(40,30,5), NANG
C  PROGRAM VERSION NUMBER, PROGRAM CONTROL VARIABLES
C.....NOISE COEFFICIENTS AND DISTRIBUTION TABLES
      COMMON /TWO/ F2D(16,6,6),
     +             FAKP(29,16,6),FAKMAP(29,16),HMYM(29,16),
     +             FAKABP(2,6),ABMAP(2,3),
     +             DUD(5,12,5),FAM(14,12),SYS(9,16,6),PERR(9,4,6)
C.....VARIABLES FROM THE DATA BASE FILE BEFORE SUNSPOT INTERPOLATION
      DIMENSION XESLCF(5,55,2), XESMCF(7,61,2), XESUCF(5,55,2),
     A XFM3CF(9,49,2),XERCOF(9,22,2),XF2COF(13,76,2),XPMAP(29,16,2)
ccc   DIMENSION STOCOF(3600),
ccc  A START(1000), END(2000)
C.....EQUIVALENCE ALL DATA FILE VARIABLES TO IFOB(40,30,5)
ccc   EQUIVALENCE(STOCOF(1), IFOB(1,1,1))
ccc   EQUIVALENCE(START(1), STOCOF(1))
ccc   EQUIVALENCE(END(1), STOCOF(1001))
ccc   EQUIVALENCE(XESMCF(1,1,1), START(1))
ccc   EQUIVALENCE(XESLCF(1,1,1), START(1))
ccc   EQUIVALENCE(XESUCF(1,1,1), END(1))
ccc   EQUIVALENCE(XFM3CF(1,1,1), START(1))
ccc   EQUIVALENCE(XERCOF(1,1,1), END(1))
ccc   EQUIVALENCE(XF2COF(1,1,1), END(1))
C--------------------------------
      CHARACTER foF2_name*12,coeff_name*12
      character(len=1), parameter :: PATH_SEPARATOR ='/'
c*****************************************************************
      nch_run=lcount(run_directory,50)
      if(coeff.ne.'URSI') coeff='CCIR'    !  default is CCIR
c          read the foF2 coefficients (CCIR or URSI88)
      write(foF2_name,'(4hfof2,a4,4h.daw)') coeff
      OPEN(27,FILE=run_directory(1:nch_run-3)//'coeffs'//PATH_SEPARATOR//foF2_name,
     +       status='old',form='unformatted',access='direct',recl=7904)
      read(27,rec=month) XF2COF
      close(27)
c*****************************************************************
c          read the rest of the coefficients
      write(coeff_name,'(5hcoeff,i2.2,5hw.bin)') MONTH
      OPEN(27,FILE=run_directory(1:nch_run-3)//'coeffs'//PATH_SEPARATOR//coeff_name,
     +       status='old',form='unformatted')
      rewind(27)
      READ(27,END=300,ERR=400) IKIM
      READ(27,END=300,ERR=400) FAKP,FAKABP
      READ(27,END=300,ERR=400) DUD,FAM,SYS
      READ(27,END=300,ERR=400) XFM3CF

      READ(27,END=300,ERR=400) F2D,PERR
      READ(27,END=300,ERR=400) ANEW,BNEW,ACHI,BCHI,FAKMAP,ABMAP
      READ(27,END=300,ERR=400) XESMCF,XPMAP
      READ(27,END=300,ERR=400) XESLCF,XESUCF
      READ(27,END=300,ERR=400) XERCOF
c*****************************************************************
      SSN100=100.-SSN
      DO 200 I=1,13
      DO 200 J=1,76
200   F2COF(I,J)=(XF2COF(I,J,1)*SSN100 + XF2COF(I,J,2)*SSN)/100.
      SSN150=150.-SSN
      SSN10=SSN-10.
      DO 210 I = 1,7
      DO 210 J = 1,61
210   ESMCOF(I,J)=(XESMCF(I,J,1)*SSN150 + XESMCF(I,J,2)*SSN10)/140.
      SSN125=125.-SSN
      SSN25=SSN-25.
      DO 220 I=1,29
      DO 220 J=1,16
220   HMYM(I,J)=(XPMAP(I,J,1)*SSN125 + XPMAP(I,J,2)*SSN25)/100.
      DO 230 I = 1,5
      DO 230 J = 1,55
      ESLCOF(I,J)=(XESLCF(I,J,1)*SSN150 + XESLCF(I,J,2)*SSN10)/140.
230   ESUCOF(I,J)=(XESUCF(I,J,1)*SSN150 + XESUCF(I,J,2)*SSN10)/140.
      DO 240 I = 1,9
      DO 240 J = 1,49
240   FM3COF(I,J)=(XFM3CF(I,J,1)*SSN100 + XFM3CF(I,J,2)*SSN)/100.
      DO 250 I = 1,9
      DO 250 J = 1,22
250   ERCOF(I,J)=(XERCOF(I,J,1)*SSN150 + XERCOF(I,J,2)*SSN10)/140.
      GO TO 500
c*****************************************************************
300   PAUSE 'END OF FILE ON DATA BASE in REDMAP'
      GO TO 500
400   PAUSE 'ERROR ON DATA BASE READ in REDMAP'
500   CLOSE(27)
      RETURN
      END
C--------------------------------
