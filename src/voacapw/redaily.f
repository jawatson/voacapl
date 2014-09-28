c# redaily.f
      SUBROUTINE REDAILY(SSN,MONTH,IDAY)
c*******************************************************************
c          Modified 12/28/94 by Greg Hand to handle daily F2
c          for Falcon AFB
c*******************************************************************
C     This routine reads and calculates the daily F2 array (F2COF)
C     FOR MONTH,day AND SUNSPOT NUMBER
C
      common /crun_directory/ run_directory
         character run_directory*50
      COMMON /ONE/ IA(6),IB(6),IKIM(10,6),ESLCOF(5,55),ESMCOF(7,61),
     +             ESUCOF(5,55),F2COF(13,76),FM3COF(9,49),ERCOF(9,22)
C.....VARIABLES FROM THE DATA BASE FILE BEFORE SUNSPOT INTERPOLATION
      DIMENSION XF2COF(13,76,2)
      DIMENSION C(9),MONN(13)
      DATA MONN /1,32,60,91,121,152,182,213,244,274,305,335,366/
c
      nch_run=lcount(run_directory,50)
c******************************************************************
      PI2=6.283185307
      DJR=(MONN(MONTH)+IDAY-16)*PI2/365.0
      DO 10 I=1,13
      DO 10 J=1,76
      F2COF(I,J)=0.0
10    CONTINUE
      C(1)=1.
      C(2)=COS(DJR)
      C(3)=SIN (DJR)
      C2=C(2)**2
      S2=C(3)**2
      C(4)=C2-S2
      C(5)=2.0*C(3)*C(2)
      C(6)=C(2)*(4.0*C2-3.0)
      C(7)=C(3)*(3.0-4.0*S2)
      C(8)=1.0+8.0*C2*(C2-1.)
      C(9)=C(5)*(4.0*C2-2.0)
      TFLUX=63.7493+0.7274*SSN+0.000895*SSN*SSN
      OPEN(27,FILE=run_directory(1:nch_run-3)//'coeffs/fof2dalw.bin',
     +                    status='old',form='unformatted')
      rewind(27)
      DO 20 KFOF2R=1,9
      READ(27,END=300,ERR=400) XF2COF
      DO 15 I=1,13
      DO 15 J=1,76
      F2COF(I,J)=F2COF(I,J)+(XF2COF(I,J,1)+XF2COF(I,J,2)*TFLUX)*
     + C(KFOF2R)
15    CONTINUE
20    CONTINUE
c**********************************************************
      GO TO 500
300   PAUSE 'End of file on DAILY data base in REDAILY'
      GO TO 500
400   PAUSE 'Error on DAILY data base read in REDAILY'
500   ITRUN=-2
      CLOSE(27)
      RETURN
      END
C--------------------------------
