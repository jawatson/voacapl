      SUBROUTINE GENOIS1(FREQ,NOISE,RLAT)
C.....VERSION 02.AUG.89  (note from 03 NOV 92 RLAT is in radians)
C.....THIS ROUTINE COMPUTES THE COMBINED NOISE DISTRIBUTION
C.....IT IS A MODIFIED VERSION OF ITS,BOULDER REP-322-3 GENOIS SUBR.
C.....NOTE HERE FOR MAN-MADE NOISE CAN BE SPECIFIED EITHER IN DBW
C.....(E.G. -151 DBW) OR ENVIRONMENT CATEGORY (NOISE=1,2,3 OR 4)
      REAL*4 XNINT(6),conn(6),DMNU(3,4),DMNL(3,4)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      COMMON /NOISE/ ATNOS,GNOS,XNOIS,SFA,ATNU,ATNY,CC,TM,KJ,JK
      COMMON /NSTATS/ DU,DL,DUA,DLA,DUM,DLM,DUG,DLG
C.....MAN-MADE NOISE LEVELS AS GIVEN BY CCIR REPORT 258.
c                   #1         #2        #3     #4     #5     #6
c               industrial residential  rural remote  noisy  quiet
      data conn/   27.7   ,   27.7    , 27.7 , 28.6 , 37.5 , 29.1 /
      DATA XNINT / 76.8   ,   72.5    , 67.2 , 53.6 , 83.2 , 65.2 /
c         -27.7 for #4 (remote) changed 4/13/07 from -28.6 per GRH to match ITU report
c          changed back 8/14/07 to match ITU Rec372 - Radio Noise.

      DATA DMNU/ 11.9,10.1,10.1,11.0,10.0, 5.9,
     1           10.9, 8.4, 9.0,10.5,10.6, 7.8/
      DATA DMNL/  9.5, 6.2, 5.1, 6.2, 5.7, 7.5,
     1            4.2, 5.0, 4.0, 7.6, 6.5, 5.5/
C.....
C.....DATA IS FA VALUES AT 1 MHZ
C.....CALCULATION OF NOISE LEVEL IS ITSA-1
C.....ATNU, ATNY ARE DB .GT. KTB FOR 1 MHZ
C.....ATNZ, ATNX ARE DB .GT. KTB FOR DESIRED FREQ,DUM
C.....ATNOS, GNOS, XNOIS ARE DB .GT. KTB FOR ALL CALCULATIONS
C.....AND ARE CONVERTED TO DBW(1 HZ BWDTH) AT END OF ROUTINE
C.....UPPER LIMIT IS 55 MHZ FOR NOISE
      DUME = AMIN1(FREQ,55.)
      MAN=NOISE
C.....FREQUENCY DEPENDENCE ATMOSPHERIC NOISE
      CALL GENFAM(RLAT,KJ,DUME,ATNU,ATNZ,DU1,DL1,SIGM,SIGU,SIGL)
      CALL GENFAM(RLAT,JK,DUME,ATNY,ATNX,DU2,DL2,SIGZ,SIGX,SIGSQ)
C.....BEGIN OF INTERPOLATION ON LOCAL TIME
      SLP = ABS(CC-TM)/4.
      ATNOS = ATNZ + (ATNX - ATNZ) * SLP
ccc      write(16,'('' In genois1='',5f10.5,2i5)') 
ccc     +       atnos,atnz,atnx,slp,rlat,kj,jk
      DUA= DU1 +(DU2-DU1)*SLP
      DLA= DL1 +(DL2-DL1)*SLP
C.....GALACTIC NOISE
      GNOS = 52. - 23. * ALOG10(FREQ)
      DUG=2.
      DLG=2.
C.....MAN MADE  NOISE
      MAN=NOISE
       IF(MAN.LE.0) THEN
C......CONVERT 3 MHZ DB .GT. 1 WATT INPUT VALUE TO FA AT 1 MHZ
        XNOIS=204.0+FLOAT(MAN)+13.22
C.......OBTAIN FA AT REQUIRED FREQUENCY
        XNOIS=XNOIS-27.7*ALOG10(FREQ)
        ICAT=2
       ELSE
C......CALC. MAN-MADE NOISE IN FA FROM NOISE ENVIRONMENT CATEGORY(1-6)
        MAN=MIN0(6,MAN)
ccc        CONN=27.7
ccc        IF(MAN.EQ.4) CONN=28.6
c**************************************************************
c          the line below was fixed 10/9/2011
c          it used to read CONN(MA)
c**************************************************************
        XNOIS=XNINT(MAN)-CONN(MAN)*ALOG10(FREQ)
        ICAT=MIN0(MAN,3)
       END IF
ccc      write(luo,1) noise,freq,xnois
ccc1     format('noise=',i5,2f10.4)
      IFN=1
      IF (FREQ.GT.3.75) IFN=2
      IF (FREQ.GT.7.5) IFN=3
      IF (FREQ.GT.15.0) IFN=4
      DUM=DMNU(ICAT,IFN)
      DLM=DMNL(ICAT,IFN)
      RETURN
      END
