c###redmap.for
      SUBROUTINE REDMAP(lu2,month,ssn)
C.....VERSION 09.01.90
C     THIS ROUTINE READS THE IONOSPHERIC LONG TERM DATA BASE FILE
      INTEGER*4 IDA(10)
C.....COEFFICIENTS FOR CRITICAL FREQUENCY FOF2 AND M(3000)F2
      COMMON/ONE/IFA(10),IMA(10),F2COF(13,76),FM3COF(9,49),SYS(9,16,6)
      COMMON /TWO /DUD(5,12,5),FAM(14,12),FAKP(29,16,6),FAKABP(2,6)
      DIMENSION XF2COF(13,76,2),XFM3CF(9,49,2)
      EQUIVALENCE (XF2COF(1,1,1),XFM3CF(1,1,1))
c*****************************************************************
      common /crun_directory/ run_directory
         character run_directory*50
      common /ccoeff/ coeff
      character coeff*4
      CHARACTER foF2_name*12,coeff_name*12
c*****************************************************************
      nch_run=lcount(run_directory,50)
      if(coeff.ne.'URSI') coeff='CCIR'    !  default id CCIR
c          read the foF2 coefficients (CCIR or URSI88)
      write(foF2_name,'(4hfof2,a4,4h.daw)') coeff
      OPEN(lu2,FILE=run_directory(1:nch_run-3)//'coeffs\'//foF2_name,
     +       status='old',form='unformatted',access='direct',recl=7904,
     +       err=900)
      read(lu2,rec=month) XF2COF
      close(lu2)
C.....FOR FOF2 ONLY SET MAX. VALUE OF SUNSPOT NO. TO 150
      SUNMAX=AMIN1(SSN,150.0)
      sun100=100.-SUNMAX
      DO 290 I=1,13
      DO 290 J=1,76
290   F2COF(I,J)=(XF2COF(I,J,1)*sun100+XF2COF(I,J,2)*SUNMAX)/100.
c*****************************************************************
c          read the rest of the coefficients
      write(coeff_name,'(5hcoeff,i2.2,5hw.bin)') MONTH
      OPEN(lu2,FILE=run_directory(1:nch_run-3)//'coeffs\'//coeff_name,
     +       status='old',form='unformatted',err=910)
      rewind(lu2)
      READ(lu2) IDA,IDA,IDA,IFA,IMA,IDA
      READ(lu2) FAKP,FAKABP
      READ(lu2) DUD,FAM,SYS
      READ(lu2) XFM3CF
      CLOSE(LU2)
c*****************************************************************
      ssn100=100.-SSN
      DO 270 I=1,9
      DO 270 J=1,49
270   FM3COF(I,J)=(XFM3CF(I,J,1)*ssn100+XFM3CF(I,J,2)*SSN)/100.
      RETURN
900   write(*,901) run_directory(1:nch_run-3)//'coeffs\'//fof2_name
901   format(' Could not OPEN file=',a)
      stop 'OPEN error in redmap at 900'
910   write(*,901) run_directory(1:nch_run-3)//'coeffs\'//coeff_name
      stop 'OPEN error in redmap at 910'
      END
c-------------------------------------------------------------
