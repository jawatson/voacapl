      SUBROUTINE FAD842
c........................................................................
C.....VERSION 01.JUNE.95
C.....Computes deviations of noise,signalpower and of S/N
c........................................................................
      INTEGER*4 PERCENT
      REAL*4 D2DVR1(2,10),D2DVR2(2,10)
      COMMON /FRQ/ FREL(12),FREL5(11,7),FW5(11,7),MAXF,MF,KOF,FREQ
     A, JKF,FXEMAX
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
C.WP3L.RG4  June 95 add 'SFA' to COMMON /NOISE/
      COMMON /NOISE/ ATMO,GNOS,XNOISE,SFA,ATNU,ATNY,CC,TM,KJ,JK
      COMMON /NSTATS/ DU,DL,DUA,DLA,DUM,DLM,DUG,DLG
      COMMON /TNOISE/BDWTH,JBW,JRSN,LUF,MAN,RLUF,RSN,XSN(2),YNOISE
      COMMON /FADE/ PERCENT,FRACX,FSX,FBX,DUSN,DLSN,SDU,SDL,SHU,SHL
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      character VERSN*40
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
c........................................................................
C.....Using the procedure of Rec. ITU-R  PI.372
C.....Comput median noise factors for atm., man-made and galactic noise
C.....at frequency FREQ
C.....(i) calc. atm. noise power from the value at 1 MHz
      IFN=MAN
      CALL GENOIS1(FREQ,IFN,RLAT)
      X10=TEN*ALOG10(BDWTH)
C.....
C.....CALC. MEDIAN POWER SUM OF THE ATM. NOISE, GAL., AND MAN-MADE NOISE
      SFA=XTEN3(TEN,ATMO,GNOS,XNOISE)
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
      DL=SFA-XTEN3(TEN,ATD1,GND1,XND1)
ccc      write(16,'('' dl='',f10.4)') dl
C.....
C.... (ii) from UPPER DECILES OF THE ATM. NOISE, GAL., AND MAN-MADE NOISE
c            calculate differences from the median
ccc      write(16,'('' dua,dug,dum='',3f10.4)') dua,dug,dum
      ATD2=ATMO+DUA
      GND2=GNOS+DUG
      XND2=XNOISE+DUM
C.....THENCE CALC. UPPER DECILE DEVIAITION OF NOISE POWER, DU
      DU=XTEN3(TEN,ATD2,GND2,XND2)-SFA
ccc      write(16,'('' du='',f10.4)') du
C.WP3L.RG4 May 95 END
c........................................................................
C.....Using the procedure of Rec. ITU-R  PI.842
C.....(i)ACCESSES 'day-to-day' AND 'within-the-hour' DECILE DEV.
C..... OF SIGNAL POWER FROM THE MONTHLY MEDIAN
      FRMUF=FREQ/FREL(12)
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
      IF(METHOD.EQ.2.OR.METHOD.EQ.3 .or. method.eq.6) THEN
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
      ELSE
        FSX=0.0
        FBX=0.0
      END IF
ccc      write(16,'('' end of FAD842, SFA,FBX='',2f10.4)') sfa,fbx
      RETURN
      END
