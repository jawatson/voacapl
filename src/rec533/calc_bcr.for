      subroutine calc_bcr
     + (it,rlatd,rlongd,freq,fmuf,icgm,bdwth,rsn,man,fracx,spwr,
     + sna,psn)
c***********************************************************************
c          Calculate the Basic Circuit Reliability (BCR) of a circuit
c          in the presence of noise.
c          This should correspond to REC ITU-R PI.842-1.
c      Given:
c            it = UT time of transmission [1-24]
c            rlatd = latitude  of receive location in degrees [N=+][-90,90]
c            rlongd= longitude of receive location in degrees [E=+][-180,180]
c            freq = Frequency of transmission [MHz]
c            fmuf = MUF of circuit [MHz]
c            icgm = was circuit above 60 latitude (for fading) [0=No,1=Yes]
c            bdwth= receiver bandwidth [Hz]
c            rsn  = required Signal-to-Noise ratio [dB]
c            man  = man-made noise level [dBw] or
c                           1 = -140.4 = industrial
c                           2 = -144.7 = residential
c                           3 = -150.0 = rural
c                           4 = -163.6 = remote
c            fracx= required circuit reliability [.1, .5=median, .9]
c            spwr = median available signal power at receive location [dBpW]
c                        (dBpW = dBw + 120.)
c      Output:
c            sna = calculated Signal-to-Noise ratio at receive location
c            psn = calculated Reliability (BCR) [.01 to .99]
c***********************************************************************
      data D2R/.01745329/
c****************************************************************
      rlat=rlatd*D2R
      rlong=rlongd*D2R
ccc      write(*,'('' rlat,tlong='',2f10.5)') rlat,rlong
c***************************************************************
      XPWR=SPWR - 120.
c***************Calc Noise**********************
C..... CALCULATE MEAN ATMOSPHERIC NOISE POWER AT 1 MHZ AT RECEIVER
      RLMT=FLOAT(IT)+RLONG/D2R/15.   !  "+" because E long is "+"
      IF(RLMT.GE.24.) RLMT=RLMT-24.
      IF(RLMT.LT.0.) RLMT=RLMT+24.
      CALL ANOIS1_x(RLMT,RLAT,RLONG)
C.....CALL FADING TO DETERMINE S/N DEVIATIONS FOR OTHER
C.....PERCENTAGES OF DAYS ON MONTH(METHODS 2,3 ONLY)
      CALL FAD842_x(freq,fmuf,man,rlat,bdwth,fracx,icgm,
     +               sfa,fbx,dusn,dlsn)
      SFA=SFA-FBX
      SN1=XPWR-SFA-10.0*ALOG10(BDWTH)+204.
      SNA=SN1
      CALL SNPROB_x(SNA,RSN,DUSN,DLSN,PSN)
ccc      write(*,'('' xpwr,sfa,fbx,bdwth,sna='',5f10.5)')
ccc     +              xpwr,sfa,fbx,bdwth,sna
      return
      end
c---------------------------------------------
      SUBROUTINE ANOIS1_x(RLMT,RLAT,RLONG)
C.....VERSION 03.NOV.92 (input lat,long now in radians)
C.....A ROUTINE THAT USES RLMT TO DETERMINE THE TIMEBLOCK (KJ)
C.....AND ADJACENT TIME BLOCK (JK) (THIS IS THE PRIOR TIMEBLOCK
C.....FOR THE FIRST 2 HOURS OF KJ, THE SAME, IE JK=KJ, FOR THE 3RD
C.....HOUR OF KJ AND THE NEXT TIME BLOCK FOR THE LAST HOUR OF KJ)
C.....AND THEN CALLS NOISY TO FIGURE THE ATMOSPHERIC NOISE (ATNU
C.....OR ATNY) FOR EACH OF THESE TIME BLOCKS.
C.....
C.....THIS ROUTINE DETERMINES THE 1 MHZ ATMOSPHERIC NOISE
C.....
C.....FOURIER SERIES IN LATITUDE AND LONGITUDE FOR TWO DISCRETE
C.....LOCAL TIME BLOCKS
C.....
      COMMON /NOISE_x/ ATNOS,GNOS,XNOIS,SFA,ATNU,ATNY,CC,TM,KJ,JK
      DATA R2D/57.295779513/
C.....LMT AT RCVR SITE
      CC = RLMT
      KJ= 6
      IF(CC.lt.22.) KJ=CC/4. + 1.
      TM = 4*KJ-2
      IF(CC-TM) 115,120,125
 115  JK = KJ -1
      GO TO 130
 120  JK = KJ
      GO TO 130
 125  JK = KJ+1
 130  IF(JK) 135,135,140
 135  JK =6
      GO TO 150
 140  IF(JK-6) 150,150,145
 145  JK = 1
C.....EAST LONGITUDE, CEG(IN DEGREES)
 150  CEG= RLONG*R2D
ccc      ceg=-ceg        !  correct because RLONG has west = + !!
      if(ceg.lt.0.) ceg=ceg+360.
      XLA =RLAT*R2D
C.....LATITUDE, XLA (IN DEGREES) "+" IS NORTH
      CALL NOISY_x(KJ,XLA,CEG,ATNU)
      CALL NOISY_x(JK,XLA,CEG,ATNY)
      RETURN
      END
c-------------------------------------------------------
      SUBROUTINE FAD842_x(freq,fmuf,man,rlat,bdwth,fracx,icgm,
     +               sfaa,fbx,dusn,dlsn)
c........................................................................
C.....VERSION 01.JUNE.95
C.....Computes deviations of noise,signalpower and of S/N
c........................................................................
      REAL*4 D2DVR1(2,10),D2DVR2(2,10)
C.WP3L.RG4  June 95 add 'SFA' to COMMON /NOISE/
      COMMON /NOISE_x/ ATMO,GNOS,XNOISE,SFA,ATNU,ATNY,CC,TM,KJ,JK
      COMMON /NSTATS_x/ DU,DL,DUA,DLA,DUM,DLM,DUG,DLG
      DATA D2DVR1/ -8.0,  6.0,-12.0,  8.0,-13.0, 12.0,-10.0, 13.0
     1           , -8.0, 12.0, -8.0,  9.0, -8.0,  9.0, -7.0,  8.0
     2           , -6.0,  7.0, -5.0,  7.0/
      DATA D2DVR2/-11.0,  9.0,-16.0, 11.0,-17.0, 12.0,-13.0, 13.0
     1           ,-11.0, 12.0,-11.0,  9.0,-11.0,  9.0, -9.0,  8.0
     2           , -8.0,  7.0, -7.0,  7.0/
      DATA SHU1,SHL1/5.0,-8.0/
      DATA TEN,A,B/ 10.0,0.9885,1.3420/
      SUM2(A1,A2)=SQRT(A1*A1+A2*A2)
      SUM3(A1,A2,A3)=SQRT(A1*A1+A2*A2+A3*A3)
      XTEN3(XX1,XX2,XX3)=
     +        TEN*ALOG10(TEN**(XX1/TEN)+TEN**(XX2/TEN)+TEN**(XX3/TEN))
c........................................................................
C.....Using the procedure of Rec. ITU-R  PI.372
C.....Comput median noise factors for atm., man-made and galactic noise
C.....at frequency FREQ
C.....(i) calc. atm. noise power from the value at 1 MHz
      IFN=MAN
      CALL GENOIS1_x(FREQ,IFN,RLAT)
      X10=TEN*ALOG10(BDWTH)
C.....
C.....CALC. MEDIAN POWER SUM OF THE ATM. NOISE, GAL., AND MAN-MADE NOISE
      SFA=XTEN3(ATMO,GNOS,XNOISE)
      sfaa=sfa
ccc      write(16,'('' after genois1='',4f9.3,2f9.5)') 
ccc     +        atmo,gnos,xnoise,sfa,rlat,rlong
ccc      write(16,'('' more='',2f10.4,2i5)') rlongd,cc,jk,kj
C.WP3L.RG4 May 95 add 27 lines
C......FROM Rec. ITU-R  PI.842 CALC:
C....(i) LOWER DECILES OF THE ATM. NOISE, GAL., AND MAN-MADE NOISE
c            calculate differences from the median
ccc      write(16,'('' dla,dlg,dlm='',3f10.4)') dla,dlg,dlm
      ATD1=ATMO-DLA
      GND1=GNOS-DLG
      XND1=XNOISE-DLM
C.....THENCE CALC. LOWER DECILE DEVIATION OF NOISE POWER, DL
      DL=SFA-XTEN3(ATD1,GND1,XND1)
ccc      write(16,'('' dl='',f10.4)') dl
C.....
C.... (ii) from UPPER DECILES OF THE ATM. NOISE, GAL., AND MAN-MADE NOISE
c            calculate differences from the median
ccc      write(16,'('' dua,dug,dum='',3f10.4)') dua,dug,dum
      ATD2=ATMO+DUA
      GND2=GNOS+DUG
      XND2=XNOISE+DUM
C.....THENCE CALC. UPPER DECILE DEVIAITION OF NOISE POWER, DU
      DU=XTEN3(ATD2,GND2,XND2)-SFA
ccc      write(16,'('' du='',f10.4)') du
C.WP3L.RG4 May 95 END
c........................................................................
C.....Using the procedure of Rec. ITU-R  PI.842
C.....(i)ACCESSES 'day-to-day' AND 'within-the-hour' DECILE DEV.
C..... OF SIGNAL POWER FROM THE MONTHLY MEDIAN
      FRMUF=FREQ/fmuf
      IF(FRMUF.LE.0.8) THEN
        J=1
      ELSE IF(FRMUF.GT.0.8.AND.FRMUF.LE.1.1) THEN
        J=2
      ELSE IF(FRMUF.GT.1.1.AND.FRMUF.LE.1.3) THEN
        J=3
      ELSE IF(FRMUF.GT.1.3.AND.FRMUF.LE.1.5) THEN
        J=4
      ELSE IF(FRMUF.GT.1.5.AND.FRMUF.LE.1.7) THEN
        J=5
      ELSE IF(FRMUF.GT.1.7.AND.FRMUF.LE.1.9) THEN
        J=6
      ELSE IF(FRMUF.GT.1.9.AND.FRMUF.LE.2.5) THEN
        J=7
      ELSE IF(FRMUF.GT.2.5.AND.FRMUF.LE.3.5) THEN
        J=8
      ELSE IF(FRMUF.GT.3.5.AND.FRMUF.LT.5.0) THEN
        J=9
      ELSE
        J=10
      END IF
ccc      write(16,'('' j='',2i5)') j,icgm
C.....DETERMINE 'day-to-day' DECILE DEV. OF PRED. MONTHLY VALUES
C.....(Table 2 Rec. ITU-R PI.842)
      IF(ICGM.LE.0) THEN
C......PATH DOES NOT CROSS 60 DEG CORR. GEOMAGNETIC LAT
        SDL=D2DVR1(1,J)
        SDU=D2DVR1(2,J)
      ELSE
C......PATH CROSSES 60 DEG CORR. GEOMAGNETIC LAT
        SDL=D2DVR2(1,J)
        SDU=D2DVR2(2,J)
      END IF
C.....DETERMINE 'within-the-hour' DECILE DEV. OF PRED. MONTHLY VALUES
      SHU=SHU1
      SHL=SHL1
ccc      write(16,'('' sdl,sdu,shu,shl='',4f10.4)') sdl,sdu,shu,shl
c........................................................................
C....  COMPUTE UPPER, LOWER DECILE DEVIATIONS OF RESULTANT
C......S/N RATIO  DUSN, DLSN resp.
C..... where DU, DL = MONTLY MEDIAN UPPER, LOWER DEV. OF NOISE PWR
C......
C..... CALCULATE ENHANCEMENT(FADING ALLOWENCE) TO ACHIEVE:
C..... (1) FIELD STRENGTH 10%,90% OF TIME  (UD and LD resp)
C..... (2) SIGNAL/NOISE 10%,90% OF TIME    (UD and LD resp)
C......NOTE: FADING ALLOWENCES ARE ONLY REQUIRED FOR S/N IN
C......METHODS 2 AND 3
       F10=SUM2(SHU,SDU)
       F90=SUM2(SHL,SDL)
ccc      write(16,'('' f10,f90='',2f10.4)') f10,f90
c......compute upper and lower decile deviations of the resultant S/N
       FB10=SUM3(SHU,SDU,DL)
       FB90=SUM3(SHL,SDL,DU)
       DUSN=FB10
       DLSN=FB90
ccc      write(16,1) freq,frel(12),frmuf,dusn,dlsn
ccc1     format(' In FAD842=',5f10.4)
c........................................................................
C.....Using the procedure of Rec. ITU-R  PI.533
C.....(i)DETERMINES S/N DEVIATIONS FOR OTHER PERCENTAGES OF DAYS
C..... OF MONTH(METHODS 2,3 ONLY) USING DEVIATIONS OF (i) AND OF
C......NOISE POWER
c........................................................................
C......DETERMINE PARAMETER C (REP. 266,EQ. 11),THENCE FADING ALLOWENCE
C......FOR FRACTION 'FRAC' OF THE TIME
        X1=ABS(FRACX-0.5)
        X2=X1*X1
C......REPRESENTATION FOR TABLE 4 OF REP 266
        FX=A*X2+B*X1
        C=EXP(FX)-1.00
        IF(FRACX.LT.0.50) THEN
          FSX=C*F10
          FBX=C*FB10
        ELSE
          FSX=-C*F90
          FBX=-C*FB90
        END IF
      RETURN
      END
c-------------------------------------------------------
      SUBROUTINE GENFAM_x(Y2,IBLK,FREQ,Z,FA,DUA,DLA,DMS,DUS,DLS)
c**********************************************************************
c          Re-written 3.June.93 by Greg Hand because previous version was
c          really incorrect. It made an attempt to limit Sigma Fam (DMS)
c          to a 10 MHz frequency, but the indicies I and J became
c          confused, and the result was not correct. Ironically,
c          Sigma Fam was the only result that was computed correctly.
c          DUA, DLA, DUS, DLS were all incorrect.  This current
c          version should limit DMS to 10 MHz and the others to 20 MHz
c          because their curves end at 20 MHz. Unfortunately, this error
c          has probably existed since time began, and it may take a
c          while for this corrected version to propagate into all version
c          that exist. The magnitude of the errors in DUA, DLA, DUS, DLS
c          could approach +/- 4dB at 20MHz.  However, for use in
c          propagation prediction programs such as IONCAP, these errors
c          are almost unnoticable because Atmospheric Noise is not
c          the driving force at HF frequencies.
c**********************************************************************
c          INPUTs:
c              Y2 = latitude of point of interest
c                   only cares if + or - to flip Northern/Southern Hemisphere
c              IBLK = time block of interest (local time 1=0000-0400)
c              FREQ = Frequency in MHz
c              Z = 1 MHz Fam value
c**********************************************************************
c          OUTPUTs:
c              FA = Frequency variation of Fam
c              DUA = Du = ratio of upper decile to median value (Fam)
c              DLA = Dl = ratio of median value (Fam) to lower decile
c              DMS = Sigma Fam = standard deviation of values of Fam
c              DUS = Sigma Du  = standard deviation of values of Du
c              DLS = Sigma Dl  = standard deviation of values of Dl
c**********************************************************************
C.....GENFAM CALCULATES THE FREQUENCY DEPENDENCE OF THE ATMOSPHERIC
C.....NOISE AND GETS DECILES AND PREDICTION ERRORS FROM TABLES
C.....
      COMMON /TWO/ DUD(5,12,5),FAM(14,12),FAKP(29,16,6),FAKABP(2,6)
      DIMENSION V(5)
      IBK=IBLK
C.....CHECK IF LATITUDE IS NORTH OR SOUTH
      IF(Y2.lt.0.) IBK=IBK+6
      U1 = -.75
      X = ALOG10(FREQ)
      U = (8. * 2.**X - 11.)/4.
      KOP = 1
 110  PZ = U1*FAM(1,IBK) + FAM(2,IBK)
      PX = U1*FAM(8,IBK) + FAM(9,IBK)
      DO 115 I=3,7
      PZ = U1*PZ + FAM(I,IBK)
 115  PX = U1*PX + FAM(I+7,IBK)
      IF(KOP.eq.1) then
         CZ = Z*PZ + PX
         CZ = Z + Z - CZ
         U1 = U
         KOP = 2
         GO TO 110
      end if
      FA = CZ*PZ + PX
c****************************************************************
c          Limit frequency to 20 MHz for DUA, DLA, DUS, DLS
c            because curves in REP 322 only go to 20 MHz
      if(FREQ.gt.20.) X=ALOG10(20.)
      DO 145 I=1,5
c          Limit frequency to 10 MHz for DMS (Sigma Fam)
c            because curves in REP 322 only go to 10 MHz
      if(I.eq.5 .and. FREQ.gt.10.) X=1.
      Y = DUD(1,IBK,I)
      DO 140 J=2,5
 140  Y = Y*X + DUD(J,IBK,I)
 145  V(I) = Y
      DUA = V(1)
      DLA = V(2)
      DUS = V(3)
      DLS = V(4)
      DMS = V(5)
      RETURN
      END
c-------------------------------------------------------
      SUBROUTINE GENOIS1_x(FREQ,NOISE,RLAT)
C.....VERSION 02.AUG.89  (note from 03 NOV 92 RLAT is in radians)
C.....THIS ROUTINE COMPUTES THE COMBINED NOISE DISTRIBUTION
C.....IT IS A MODIFIED VERSION OF ITS,BOULDER REP-322-3 GENOIS SUBR.
C.....NOTE HERE FOR MAN-MADE NOISE CAN BE SPECIFIED EITHER IN DBW
C.....(E.G. -151 DBW) OR ENVIRONMENT CATEGORY (NOISE=1,2,3 OR 4)
      REAL*4 XNINT(4),DMNU(3,4),DMNL(3,4)
      COMMON /NOISE_x/ ATNOS,GNOS,XNOIS,SFA,ATNU,ATNY,CC,TM,KJ,JK
      COMMON /NSTATS_x/ DU,DL,DUA,DLA,DUM,DLM,DUG,DLG
C.....MAN-MADE NOISE LEVELS AS GIVEN BY CCIR REPORT 258.
      DATA XNINT /76.8, 72.5, 67.2, 53.6/
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
      CALL GENFAM_x(RLAT,KJ,DUME,ATNU,ATNZ,DU1,DL1,SIGM,SIGU,SIGL)
      CALL GENFAM_x(RLAT,JK,DUME,ATNY,ATNX,DU2,DL2,SIGZ,SIGX,SIGSQ)
C.....BEGIN OF INTERPOLATION ON LOCAL TIME
      SLP = ABS(CC-TM)/4.
      ATNOS = ATNZ + (ATNX - ATNZ) * SLP
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
C......CALC. MAN-MADE NOISE IN FA FROM NOISE ENVIRONMENT CATEGORY(1-4)
        MAN=MIN0(4,MAN)
        CONN=27.7
        IF(MAN.EQ.4) CONN=28.6
        XNOIS=XNINT(MAN)-CONN*ALOG10(FREQ)
        ICAT=MIN0(MAN,3)
       END IF
      IFN=1
      IF (FREQ.GT.3.75) IFN=2
      IF (FREQ.GT.7.5) IFN=3
      IF (FREQ.GT.15.0) IFN=4
      DUM=DMNU(ICAT,IFN)
      DLM=DMNL(ICAT,IFN)
      RETURN
      END
C------------------------------------------------------------------
c###noisy.for
      SUBROUTINE NOISY_x (KJ, XLA, CEG, ANOS)
CR....A ROUTINE TO USE THE TIMEBLOCK (KJ), THE LAT (XLA), THE LONG
CR....(CEG), AND THE IONOSPHERIC COEFFICIENTS (FAKP AND FAKAB) TO
CR....DETERMINE THE ATMOSPHERIC NOISE (ANOS).
C.....NOISY IS A GENERAL PURPOSE ROUTINE USED TO EVALUATE A FOURIER
C.....SERIES IN TWO VARIABLES.
C.....KJ --- NUMBER OF FOURIER COEFFICIENT ARRAY TO BE USED
C.....XLA --- GEOGRAPHIC LATITUDE, DEGREES,
C.....CEG --- GEOGRAPHIC EAST LONGITUDE, DEGREES
C.....ANOS --- NOISE VALUE, MEDIAN POWER DB ABOVE KTB
C.....FAKABP --- NORMALIZING FACTORS FOR FOURIER SERIES
C.....KJ = 1 TO 6 IS ATMOSPHERIC NOISE
C.....
C.....* NOTE - XLA, CEG, ANOS, FAKABP ARE NOT ALWAYS AS PREVIOUSLY
C.....         DEFINED
C.....FOURIER VARIABLES AND ATMOSPHERIC RADIO NOISE
C.....
      COMMON /TWO/ DUD(5,12,5),FAM(14,12),FAKP(29,16,6),FAKABP(2,6)
      DIMENSION SX (15), SY(29), ZZ (29)
      IF (KJ.gt.6) KJ=6
C.....LIMITS OF FOURIER SERIES
      LM = 29
      LN = 15
C.....HALF ANGLE (IN RADIANS)
      Q = .0087266466 * CEG
C.....LONGITUDE SINES
      DO 115 K = 1, 15
 115  SX(K)=SIN(Q*K)
C.....LONGITUDE SERIES
      DO 125 J = 1, LM
      R = 0.
      DO 120 K = 1, LN
 120  R = R + SX (K) * FAKP (J, K, KJ)
 125  ZZ (J) = R + FAKP (J, 16, KJ)
C.....ANGLE PLUS 90 DEGREES (IN RADIANS)
      Q = .01745329252 * (XLA + 90.)
C.....LATITUDE SERIES
      DO 140 J=1,29
 140  SY(J)=SIN(Q*J)
      R = 0.
      DO 130 K = 1, LM
 130  R = R + SY (K) * ZZ (K)
C.....FINAL FOURIER SERIES EVALUATION (NOTE LINEAR NORMALIZATION)
      ANOS = R + FAKABP(1,KJ)+FAKABP(2,KJ)* Q
      RETURN
      END
c-------------------------------------------------------------
c###snprob.for
      SUBROUTINE SNPROB_x(SN,SN0,XU,XL,PR)
C ------------------------------------------------------
C      SUBROUTINE SNPROB:
C..... VERSION 04.JAN.88
C         CALCULATES PROBABILITY OF GIVEN SIGNAL/NOISE LEVEL
C ------------------------------------------------------
      IF(SN-SN0)10,4,4
    4 PP=130.0-80.0/(1.0+(SN-SN0)/XL)
      PP=AMIN1(PP,99.0)
      GO TO 12
   10 PP=80.0/(1.0+(SN0-SN)/XU)-30.0
      PP=AMAX1(PP,1.0)
   12 PR=PP*0.01
      RETURN
      END
c-------------------------------------------------------------
