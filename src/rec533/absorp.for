c*********************************************************************
      SUBROUTINE ABSORP(FREQ,SECI,KM,ABSO)
C     MID,RAL mods 30.01.95 to remove non std. Fortran 77 statements
      CHARACTER*40 VERSN
      COMMON /CON/D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON/GEOG/ABIY(5),CLAT(5),CLONG(5),CLCK(5),EPSPAT(5),F2M3(5)
c>>(b)WP-6A SEPT. 93 MODS add 2 lines REMOVE 1 LINE
     A,FI(3,5),GLAT(5),GMDIP(5),GYZ100(5),GYR100,GYZ300(5),HPF2(5),RD(5)
     B,SIGPAT(5),CYCEN(5)
CX   A,FI(3,5),GLAT(5),GMDIP(5),GYZ(5),HPF2(5),RD(5),SIGPAT(5),CYCEN(5)
c>>(b)WP-6A SEPT. 93 MODS end of change
      COMMON /ABSOR/ GYL(5),TAF(5),PEXP(5),ASSN
      ABSO = 0.
      DO 1 K = 1, KM
        IF (CYCEN(K).LE.102.) THEN
          CHI = CYCEN(K)*D2R
          FCHI = MAX ((COS(0.881*CHI))**PEXP(K), 0.02)
         ELSE
          FCHI=.02
        END IF
        T = FREQ/(SECI*FI(1,K))
        CALL PHIFUN (T,PHI)
        A = TAF(K) * ASSN * FCHI * PHI/.34 * SECI / ((FREQ+GYL(K))**2)
C.....  NORMALIZATION BY FCHInoon
        CHI = MIN (CY12(K),102.) * D2R
        FCHInoon = COS(.881*CHI)**PEXP(K)
        A = A / FCHInoon
        ABSO = ABSO + A
    1 CONTINUE
      ABSO = ABSO / KM
      RETURN
      END
c**************************************************************************
