c###prbmuf.for
      FUNCTION PRBMUF(FMHZ,FGO,FSET,IL)
C--------------------------------
C
C     THIS ROUTINE IS THE STANDARD PROBABILITY EVALUATION FOR THE
C     PROGRAM. THE DISTRIBUTIONS ARE SET IN SUBROUTINE CURMUF
C
C
C  EVALUATE THE PROBABILITY THAT THE OPERATING FREQUENCY IS GREATER
C THAN THE FOT, MUF OR HPF FOR A GIVEN LAYER USING THE MUF DISTRIBUTIONS
C     PRBMUF = 1. - FNORML(Z)
C
C  FMHZ IS THE OPERATING FREQUENCY
C  FGO IS THE MUF AT A SET ANGLE FOR A PARTICULAR LAYER, FOR ONE HOP
C      (IT MAY NOT BE THE CIRCUIT MUF)
C  FSET IS WHERE MEDIAN OF DISTRIBUTION IS PLACED (=FOT, MUF OR HPF)
C  IL IS LAYER (E, F1, F2 OR ES)
C  SIGL(IL) IS THE LOWER DECILE
C  SIGU(IL) IS THE UPPER DECILE
C
      COMMON / MUFS / EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24),
     A FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),
     B DELMUF(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),
     C YHPF(4),YMUF(4)
C
      DIMENSION C(4)
      DATA C/.196854, .115194, .000344, .019527/
C
      Z = FMHZ - FGO
      IF(FSET.gt.0.) go to 95
      IF(Z.le.0.) then
         QX = 1.
      else
         QX = 0.0
      end if
      GO TO 145
   95 IF(Z.le.0.) then
         SIG = FGO * SIGL(IL) / FSET
      else
         SIG = FGO * SIGU(IL) / FSET
      end if
      SIG = AMAX1(SIG,0.001)
      Z = Z / SIG
C.....KEEP WITHIN 32 BIT WORD MACHINES
C.....NOTE PRBMUF = 1. - FNORML(Z)
      YP = Z
      YPX = Z
      IF(YPX.lt.0.) YP=-YPX
      YP = AMIN1(5.0, YP)
      QX = 1. + YP * (C(1) + YP * (C(2) + YP* (C(3) + YP * C(4))))
      QX = QX * QX * QX * QX
      QX = 1. / QX
      QX = .5 * QX
      IF(YPX.lt.0.) then
         PX = QX
         QX = 1. - PX
      else
         PX = 1. - QX
      end if
  145 PRBMUF = QX
      RETURN
      END
CC*****************************************************************
