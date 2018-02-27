c# genfam.f
C--------------------------------
      SUBROUTINE GENFAM(Y2,IBLK,FREQ,Z,FA,DUA,DLA,DMS,DUS,DLS)
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
C--------------------------------
C
C     GENFAM CALCULATES THE FREQUENCY DEPENDENCE OF THE ATMOSPHERIC
C     NOISE AND GETS DECILES AND PREDICTION ERRORS FROM TABLES
C
      COMMON / TWO / F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     A FAM(14,12), SYS(9,16,6), PERR(9,4,6)
      DIMENSION V(5)
C.....
      IBK=IBLK
C.....CHECK IF LATITUDE IS NORTH OR SOUTH
      IF(Y2.lt.0.) IBK=IBK+6
      U1 = -.75
      X = ALOG10(FREQ)
      U = (8. * 2.**X - 11.) /4.
      KOP = 1
 110  PZ = U1*FAM(1,IBK) + FAM(2,IBK)
      PX = U1*FAM(8,IBK) + FAM(9,IBK)
      DO 115 I=3,7
      PZ = U1*PZ + FAM(I  ,IBK)
 115  PX = U1*PX + FAM(I+7,IBK)
      IF(KOP.eq.1) then
         CZ = Z * PZ + PX
         CZ = Z + Z - CZ
         U1 = U
         KOP = 2
         GO TO 110
      end if
      FA = CZ*PZ + PX
c***********************************************************
c          Limit frequency to 20 MHz for DUA, DLA, DUS, DLS
c            because curves in REP 322 only go to 20 MHz
      if(FREQ.gt.20.) X=ALOG10(20.)
      DO 145 I = 1, 5
c          Limit frequency to 10 MHz for DMS (Sigma Fam)
c            because curves in REP 322 only go to 10 MHz
      if(I.eq.5 .and. FREQ.gt.10.) X=1.
      Y = DUD(1,IBK,I)
      DO 140 J = 2,5
 140  Y = Y*X + DUD(J,IBK,I)
 145  V(I) = Y
      DUA = V(1)
      DLA = V(2)
      DUS = V(3)
      DLS = V(4)
      DMS = V(5)
      RETURN
      END
