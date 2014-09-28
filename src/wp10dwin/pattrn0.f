      SUBROUTINE PATTRN0(AZIM,ELEV,XGN,FACTOR)
C--------------------------------------------------------------------
C                        SUBPROGRAM USAGE
C
C  SGAIN - computes power gain dBi
C*********************************************************************
C  COMMON BLOCKS:
C ANTDAT
C       nostak - NUMBER OF VERTICAL STACKS
C       STKSPM - SPACING BETWEEN STACKS
C       NUMBAY - NUMBER OF BAYS
C       BAYSPM - SPACING BETWEEN BAYS
C       DIPLNM - LENGTH OF DIPOLE RADIATOR
C       RRSPM - SPACING BETWEEN RADIATORS AND REFLECTING SCREEN
C       STKHTM - HEIGHT OF LOWEST STACK ABOVE GROUND
C       STKRAT - CURRENT RATIO FOR STACKS
C       BAYPHS - RELATIVE PHASE FOR THE BAYS
C       BAYRAT - CURRENT RATIO FOR THE BAYS (ALWAYS = 1)
C       OFMHZ - ANTENNA OPERATING FREQUENCY
C       DFMHZ - ANTENNA DESIGN FREQUENCY
C
C CONST
C       PI      -  MATHEMATICAL SYMBOL PI = 3.14159
C       VOFL    -  VELOCITY OF LIGHT  FOR FREQUENCY IN MEGAHERTZ = 3.0E-
C       PI2     -  2 * PI
C       PIO2    -  PI / 2
C       D2R     -  CONVERSION FACTOR FOR DEGREES TO RADIANS CONVERSION
C                   = PI / 180.
C       R2D     -  CONVERSION FACTOR FOR RADIANS TO DEGREES CONVERSION
C                   = 180 / PI
C FWAVE
C       EIL     -  0.5 * LENGTH OF DIPOLE IN radianS
C       XB      -  SPACING BETWEEN BAYS IN radiansS
C       XS      -  SPACING BETWEEN STACKS IN radianS
C       XH      -  HEIGHT OF LOWEST STACK ABOVE GROUND IN radianS
C       XR      -  REFLECTOR TO DIPOLE SPACING IN radianS
C       C(8)    -  ARRAY OF RELATIVE STACK CURRENT MAGNITUDE RATIOS
C       R(14)   -  ARRAY OF RELATIVE BAY CURRENT MAGNITUDE RATIOS
C       PS(14)  -  ARRAY OF BAY CURRENT PHASE RATIOS IN RADIANS INCLUDIN
C                     FREQUENCY ADJUSTMENT FOR OPERATING FREQUENCY NOT
C                     EQUAL TO DESIGN FREQUENCY
C       A(8)    -  ARRAY OF STACK CURRENT PHASE RATIOS IN RADIANS,
C                    CURRENTLY DEFAULTED TO ZERO
C       Y(14)   -  ARRAY CONTAINING THE SPACINGS BETWEEN THE BAY DIPOLES
C                     IN radianS (RELATIVE TO BAY 1)
C       Z(8)    -  ARRAY CONTAINING THE SPACINGS BETWEEN THE STACK DIPOL
C                     IN radianS (RELATIVE TO STACK 1)
C       WAVE    -  OPERATING FREQUENCY WAVELENGTH
C       BETA    -  OPERATING FREQUENCY WAVE NUMBER (K=2*PI/WAVE)
C
C
C  DUMMY ARGUMENTS:
C       AZIM  - an azimuth counter-clockwise from Boresight RADIANS 
C       ELEV  - an elevation angle above the horizon  RADIANS
C       XGN  - antenna gain (dB) for AZIM, ELEV
C       FACTOR  - integral result for antenna gain normalization
C
C*********************************************************************
      COMMON/ANTDAT/nostak,STKSPM,NUMBAY,BAYSPM,DIPLNM,RRSPM,STKHTM,
     +STKRAT(8),bayphs(14),DFMHZ,OFMHZ
      COMMON/FWAVE/EIL,CEIL,XB,XS,XH,XR,C,R,PS,A,Y,Z,WAVE,BETA
      DIMENSION C(8),R(14),PS(14),A(8),Y(14),Z(8)
      DATA PI/3.1415926/,VOFL/299.79246/
      DATA PI2/6.283185307/
      DATA D2R/.01745329251/
C  SET DEFAULT VALUES FOR GAIN
        WAVE = VOFL /OFMHZ
        BETA = PI2 / WAVE
        XH = STKHTM * BETA
        XB = BETA * BAYSPM
        EL = DIPLNM * BETA
        EIL = EL / 2.0
        CEIL = COS(EIL)
        XR = RRSPM * BETA
        XS = STKSPM * BETA
C
C  CONSTANTS FUNCTIONS OF FREQUENCY ONLY
       DO 30 Is = 1,nostak
         Z(Is) = XS * FLOAT(Is-1) + XH
         C(Is) = STKRAT(Is)
   30  CONTINUE
       ODRAT = D2R * OFMHZ / DFMHZ
       NBS=IABS(NUMBAY)
       DO 40 Ib = 1,NBS
          PS(Ib) = ODRAT * bayphs(Ib)
          R(Ib) = 1.
          Y(Ib) = FLOAT(Ib-1) * XB
   40 CONTINUE
        Y(1)=0.
        IF(FACTOR.EQ.-99999.)THEN
          CALL DBLTRAP(DINTGL)
          FACTOR = 4.0 * PI / DINTGL
        ENDIF
        XGN=-1000.
        XGAIN = F2 (ELEV,AZIM)
        IF(XGAIN.NE.0.) XGN = 10.* ALOG10(ABS(XGAIN*FACTOR))
        RETURN
      END
C------------------------------------------------------------------
