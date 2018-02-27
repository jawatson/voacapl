c###serprb.for
      SUBROUTINE SERPRB(IF)
C--------------------------------
C
C     THIS ROUTINE FINDS THE SERVICE PROBABILITY
C
C     THIS ROUTINE HAS NOT BEEN REVISED TO USE SUMMED SIGNAL
C     TLLOW(IM) = DBLOSL(IF)
C     SN(IM) = SNDB(IF)
C     TLHGH(IM) = DBLOSU(IF)
C     IM = IF
C     DO ONLY ONCE FOR IF
C
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SYGU,SYGL,KJ,JK
      COMMON / CON / D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / SON / ANGLE(13), ANGLER(13), CPROB(13), DBLOS(13),
     A DBLOSL(13), DBLOSU(13), DBU(13), DELAY(13), DBW(13), NHP(13),
     B XNYNOIS(13), PROBMP(13), RELIAB(13), SNDB(13), SNPR(13),
     C SNRLW(13), SNRUP(13), SPROB(13), VHIGH(13), RNEFF(13),MDL(13)
       CHARACTER MDL*1
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
      COMMON / allMODE /ABPS(20),CREL(20),FLDST(20),HN(20),HP(20),
     1PROB(20),RELY(20),RGAIN(20),SIGPOW(20),SN(20),
     2SPRO(20),TGAIN(20),TIMED(20),TLOSS(20),B(20),FSLOS(20),
     3grlos(20),adv(20),obf(20),
     CNMODE(20),TLLOW(20),TLHGH(20),EFF(20),NREL,NMMOD
      DIMENSION TME(10),d10sa(20),d50sa(20)
C.....NORMAL DISTRIBUTION
      DATA TME/0.0,0.1257,0.2533,0.3853,0.5244,0.6745,0.8416,1.0364
     A , 1.2815, 1.6449 /
C.....DR IS THE PREDICTION ERROR IN RSN, REQUIRED SNR
      DATA DR/2./
      SPR = 0.0
C.....LUFP IS THE REQUIRED RELIABILITY
      ITM = IABS( LUFP - 50) /5 +1
      ITM = MIN0(ITM,10)
      TMX = TME(ITM)
C.....BEGINNING OF FINDING THE SERVICE PROBABILITY FOR EACH MODE
      DO 145 IM = 1,NMMOD
      IF(HP(IM).le.70.) then     !  set default for no mode
         SPRO(IM) = .001
         d10sa(im)=0.
         d50sa(im)=0.
         GO TO 145
      end if
      IF(LUFP.ge.50) then
         DN  = TMX *DU/DCL                !  DN is the noise
         DS  = TMX *TLLOW(IM)/DCL         !  DS is the signal
         XLH = -1.
C.....SIGNAL PREDICTION ERROR
         DSO = TMX *SUS
         DNO = TMX * SYGU/DCL
      else
         DN  = TMX *DL/DCL                !  DN is the noise
         DS  = TMX *TLHGH(IM)/DCL         !  DS is the signal
         XLH = 1.
         DSO = TMX * SLS
         DNO = TMX *SYGL/DCL
      end if
C.....D10S IS THE LOWER DECILE
C.....D50S IS THE MEDIAN
C.....D90S IS THE UPPER DECILE
      D50Sa(im)= SQRT(DN*DN + DS*DS)
      D10Sa(im)= D50Sa(im) + 
     +        SQRT(SIGM*SIGM + ADS*ADS + DNO*DNO + DSO*DSO + DR*DR)
      D50Sa(im) =  SN(IM)  + XLH*D50Sa(im)
      Z = (RSN - D50Sa(im))/d10sa(im)
C.....SERVICE PROBABILITY FOR THIS MODE
      SPRO(IM) =  1. - FNORML(Z)
  145 CONTINUE
C.....END OF FINDING THE SERVICE PROBABILITY FOR EACH MODE
C
C.....USE THE MAXIMUM
      AMXX=-1000.
      imax=1
      DO 150 I=1,NMMOD
      IF(SPRO(I).GT.SPRO(imax)) imax=i
  150 CONTINUE
      SPROB(IF)=SPRO(imax)
      D10S=d10sa(imax)
      D50S=d50sa(imax)
      D90S=D10S
      RETURN
      END
C--------------------------------
