      SUBROUTINE MUF434(foF2,X,XM3,FH,D,JHOP,KC,DMX0,F2DMUF)
C -----------------------------------------------------
C         SUBROUTINE MUF434:
C         CALCULATES F2(D)MUF
C         VERSION 03.Nov.93
C -----------------------------------------------------
C....
c>>(e)WP-6A SEPT. 93 MODS REPLACE INPUT 'KF' BY 'KC' THROUGHOUT SUBROUTINE
C....COMPUTER EVALUTION OF REC 434 PROCEDURE
C....INPUT:  X = FOF2/FOE, XM3=M(3000)F2 , FH=GYROFREQUENCY
C....        D = HOP RANGE
C....     JHOP = HOP COUNT/CONTROL PARAMETER
C....       KC = CONTROL PT. INDICATOR
C....OUTPUT: DMX0  = MAX. RANGE ( 0.0 deg. elev)
C....        F2DMUF = MUF(D)F2
C....
      save B,DMX,C3000
      REAL*4 B(5),DMX(5)
      CDF(Z)=0.74-0.591*Z-0.424*Z*Z-0.09*Z**3+0.088*Z**4+0.181*Z**5
     1      +0.096*Z**6
C.....
C.....CALC. VALUES OF FOF2/FOE AMD M(3000)F2 AT SELECTED CONTRL
C.....POINT OF TABLE 1(i) of REC. 533
      F2DMUF=0.0
      X=AMAX1(X,2.0)
      X2=X*X
      X4=X2*X2
      X6=X4*X2
      XM32=XM3*XM3
      IF(JHOP.LE.1) THEN
C.....NEED ONLY EVALUATE B,DMX AND C3000 ON FIRST ENTRY AT A GIVEN TIME
C.....i.e FOR LOWEST ORDER ONLY AS THERE IS NO RANGE DEPENDENCE
        B(KC)=XM3-0.124+(XM32-4.0)*(0.0215+0.005*SIN(7.854/X-1.9635))
        DMX(KC)=4780.0+(12610.0+2140.0/X2-49720.0/X4+688900.0/X6)
     1  *(1.0/B(KC)-0.303)
        IF(JHOP.EQ.0) THEN
C........FOR JHOP =0 ONLY CALC. OF DMX0 is required
          DMX0=DMX(KC)
          RETURN
        END IF
        Z3000=1.0-6000.0/DMX(KC)
        C3000=CDF(Z3000)
      END IF
      DMX0=DMX(KC)
      Z=1.0-2.0*D/DMX0
      CD=CDF(Z)
      F2DMUF=(1.0+(B(KC)-1.0)*CD/C3000)*foF2+0.5*FH*(1.0-D/DMX0)
      RETURN
      END
