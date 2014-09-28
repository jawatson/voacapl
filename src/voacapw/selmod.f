c###selmod.for
      SUBROUTINE SELMOD
C--------------------------------
C
C     THIS ROUTINE SELECTS THE SAMPLE AREA TO BE USED IN RAY PATH
C     CALCULATIONS.
C
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      IF(KFX.EQ.1)THEN
C.......ONLY ONE AVAILABLE
        JMODE =1
      ELSE IF(KFX.EQ.2)THEN
C.......ONLY TWO WITH THE SAME F (SET EQUAL IN IONSET)
C.......SO CHECK E LAYER
        IF(FI(1,1).GT.FI(1,2))THEN
C.........RECEIVER END CONTROLS
          JMODE =2
        ELSE
C.........TRANSMITTER END CONTROLS
          JMODE =1
        ENDIF
      ELSE
C.......THREE SAMPLES BUT CHECK ENDS ONLY
C.......CHECK  F2 LAYERS
        DELFI=FI(3,1)-FI(3,3)
        IF( ABS(DELFI).LT.0.01 )THEN
C.........F2 LAYER IS SAME SO CHECK E LAYER
          IF(FI(1,1).GT.FI(1,3))THEN
C...........RECEIVER END CONTROLS
            JMODE =3
          ELSE
C...........TRANSMITTER END CONTROLS
            JMODE =1
          ENDIF
        ELSE
C.........F2 LAYER CONTROLS
          IF(DELFI.GT.0.)THEN
C...........RECEIVER END CONTROLS
            JMODE =3
          ELSE
C...........TRANSMITTER END CONTROLS
            JMODE =1
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
C--------------------------------
