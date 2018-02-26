      FUNCTION F2(THETA,PHI)
C*********************************************************************
C  PARAMETERS:
C    THETA - takeoff angle in radians
C    PHI - azimuthal angle in radians
C*********************************************************************
      COMMON/ANTDAT/nostak,STKSPM,NUMBAY,BAYSPM,DIPLNM,RRSPM,STKHTM,
     +STKRAT(8),bayphs(14),DFMHZ,OFMHZ
      DIMENSION C(8),R(14),PS(14),A(8),Y(14),Z(8)
      COMMON/FWAVE/EIL,CEIL,XB,XS,XH,XR,C,R,PS,A,Y,Z,WAVE,BETA
         CPHI = COS(PHI)
         SPHI = SIN(PHI)
         CTHETA = COS(THETA)
         STHETA = SIN(THETA)
         CPSI = CTHETA * SPHI
         SPSI2 = 1. - CPSI * CPSI
         IF( SPSI2  .LE. 1.0E-12 ) SPSI2 = 1.0E-12
         EF = (COS(EIL * CPSI) - CEIL) / SPSI2
         FZPHR=0.
         FYR=0.
         FYI=0.
         NBS=IABS(NUMBAY)
         DO 50 I  = 1,NBS
         ARG = Y(I) * CPSI + PS(I)
         FYR=FYR+R(I)*COS(ARG)
   50    FYI=FYI+R(I)*SIN(ARG)
         FX =  SIN( XR * CTHETA * CPHI)
         DO 60 I =1,nostak
           COF =  C(I) * SIN(Z(I)* STHETA)
           FZPHR=FZPHR+COF
   60    CONTINUE
         COF = STHETA * SPHI
         FZTHR=FZPHR*COF
         FZPHR=FZPHR*CPHI
         F2 =  EF*EF  *FX*FX  *(FYR*FYR +FYI*FYI)
     1     *(FZTHR*FZTHR +FZPHR*FZPHR)
      RETURN
      END
ccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
