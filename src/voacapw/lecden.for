c###lecden.for
      SUBROUTINE LECDEN(K)
C--------------------------------
C
C     THIS ROUTINE GENERATES ELECTRON DENSITY PROFILE
C
C     ADD TOP BY EXTENDING HTR(50,3) AND FNSQ(50,3)
C
      COMMON / MFAC / F2M3(5),HPF2(5),ZENANG(5),ZENMAX(5),IEDP,FSECV(3)
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
C.....RETURN IF EXTERNAL ELECTRON DENSITY PROFILE IS USED
      DO 90 I = 1,3
      IF(IELECT(I)) 90, 90, 85
   85 RETURN
   90 CONTINUE
C.....MUST CHECK ON F1 LAYER PARAMETERS FIRST
      XLOW = .8516
      HZ = HI (1, K) - YI (1, K)
      XUP = 0.98 * FI (1, K) / FI (3, K)
      HLOW = HZ + YI (1, K) * (1. + SQRT (1. - XLOW * XLOW))
      HTE = HI (1, K) + YI (1, K)
      HB2 = HI (3, K) - YI (3, K)
      FCE = FI (1, K) * FI (1, K)
      FC2 = FI (3, K) * FI (3, K)
C.....VALLEY FILLED FROM (FLOW,HLOW) TO (FUP,HUP)
      HUP = HB2 + YI (3, K) * (1. - SQRT (1. - XUP * XUP))
      FUP = XUP * XUP * FC2
      FLOW = XLOW * XLOW * FCE
      ASP = 0.
      IF (HUP - HLOW)175, 175, 165
  165 ASP = (FUP - FLOW) / (HUP - HLOW)
  175 CONTINUE
      IF (FI (2, K))215, 215, 185
  185 CONTINUE
      FC1 = FI (2, K) * FI (2, K)
      HB1 = HI (2, K) - YI (2, K)
      HT1 = HI (2, K) + YI (2, K)
C.....HEIGHT OF F2 AT F1 CRITICAL FREQUENCY
      HTW = HB2 + YI (3, K) * (1. - SQRT (1. - FC1 / FC2))
c-----
ccc      WRITE(6,'(1h+)')
c-----
ccc      IF (HTW - HI (2, K))190, 190, 205
      IF(HTW.gt.HI(2, K)+.001) go to 205
C.....FORCE F1 AT CRITICAL FREQUENCY
  190 YS = HTW - HB1
      YS = AMAX1 (1., YS)
C.....SLOPE OF LINEAR F1
      S1 = FC1 / YS
      hiold=hi(2,k)
      HI(2,K) = HTW
      YI(2,K) = YS
C.....AVOID A SPURIOUS LAYER
      IF(HB2 - HB1) 194, 195, 195
  194 YI(2,K) = HI(2,K) - HB2
      GO TO 205
C.....SET FLAG TO INDICATE LINEAR LAYER FOR F1
  195 LIN = 1
      FSECV(K) =  SQRT(FLOW)
      DENOM = 1. - (FI(1,K) / FI(2,K)) ** 2
C.....F1 LINE NOT TO OBSCURE E LAYER
      DENOM = AMAX1(.17,DENOM)
      YB = (HI(2,K) - HI(1,K)) / DENOM
      IF(YS-YB) 402,401,401
C  THIS  IS CCIR(1976) IF FI(2,K) = 1.7*FI(1,K)
C.....F1 PASSES THROUGH E NOSE
  401 YS = YB
      YI(2,K) = YS
      S1  = FC1/YS
      HB1 = HI(1,K)
      GO TO 415
  402 CONTINUE
C.....TO FORCE CCIR (BRADLEY) SET
C     FI(2,K) = 1.7 * FI(1,K)
C     HI(2,K) = HTW
C     YI(2,K) = YB
C     HB1 = HI(1,K)
C     HT1 = HTW
C     S1  = FC1/YS
  415 CONTINUE
      HT1 = HTW
      GO TO 215
  205 LIN = 0
C.....FORCE F1 ABOVE E LAYER
      YI(2,K) = AMIN1(YI(2,K),(HI(2,K) - HI(1,K) + 1.))
      FSECV(K) = -1.
  215 CONTINUE
C.....D-REGION PROFILE - XTAIL MAY HAVE DIURNAL VARIATION.
      HD = 70.
      XTAIL = .85
      HEX = HI(1,K) - XTAIL * YI(1,K)
      FNX = 1. - XTAIL * XTAIL
C.....SLOPE OF E IS SAME AS SLOPE OF V AT HEX
      ALP = 2. * (HI(1,K) - HEX) / (FNX * YI(1,K) * YI(1,K))
      FSQ = FNX * EXP(-ALP * (HEX - HD))
      HTR(1,1) = HD
      HTR(5,1) = HEX
      HDIF = (HTR(5,1) - HTR(1,1)) * .25
      HDIF = AMAX1 (0., HDIF)
      HTR(4,1) = HTR(5,1) - AMIN1(1.0,HDIF)
      HTR(2,1) = HTR(1,1) + HDIF
      HTR(3,1) = (HTR(2,1) + HTR(4,1)) * 0.5
C  E  BELOW NOSE
      HTR(11,1) = HI (1, K)
      HDIF = (HTR(11,1) - HTR(5,1)) / 6.
      DO 100 IH = 6, 10
  100 HTR(IH,1) = HTR(IH - 1,1) + HDIF
C  E ABOVE  NOSE
      HTR(17,1) = HI (1, K) + YI (1, K)
      HDIF = (HTR(17,1) - HTR(11,1)) / 6.
      DO 105 IH = 12, 16
  105 HTR(IH,1) = HTR(IH - 1,1) + HDIF
      HTR(11,1) = 0.5 * (HTR(10,1) + HTR(12,1))
      HTR(50,1) = HI (3, K)
      IF (FI (2, K))110, 110, 125
C  F2  LAYER, NO F1 LAYER.
  110 HTR(18,1) = HI (3, K) - YI (3, K)
      HDIF = (HTR(50,1) - HTR(18,1)) / 32.
      DO 115 IH = 19, 49
  115 HTR(IH,1) = HTR(IH - 1,1) + HDIF
      GO TO 155
C  F1 LAYER AND F2 LAYER
  125 HB2 = HI(3,K) - YI(3,K)
      HB1 = HI(2,K) - YI(2,K)+.00001
      IF(HB2 - HB1) 110, 110, 126
  126 HTR(18,1) = HI(2,K) - YI(2,K)
      HTR(18,1) = AMAX1 (HTR(18,1), HTR(17,1) + 1.)
      HTR(28,1) = HI (2, K)
      HDIF = (HTR(28,1) - HTR(18,1)) / 10.
      DO 135 IH = 19, 27
  135 HTR(IH,1) = HTR(IH - 1,1) + HDIF
      HDIF = (HTR(50,1) - HTR(28,1)) / 22.
      DO 145 IH = 29, 49
  145 HTR(IH,1) = HTR(IH - 1,1) + HDIF
  155 CONTINUE
C.....FORCE F1 ABOVE E LAYER
      HB1 = AMAX1(HI(1,K), (HI(2,K)-YI(2,K) )  )
      DO 375 IH = 1, 50
      FND = 0.
      FNE = 0.
      FN1 = 0.
      FN2 = 0.
      FNVAL = 0.
      H = HTR(IH,1)
      IF (H - HLOW)245, 245, 225
  225 IF (H - HUP)235, 245, 245
C.....LINEAR VALLEY
  235 FNVAL = FUP + ASP * (H - HUP)
  245 IF(H - HEX) 275, 255, 255
  255 IF (H - HTE)260, 260, 285
C.....PARABOLIC E
  260 Z = (H - HI (1, K)) / YI (1, K)
      FNE = FCE * (1. - Z * Z)
      IF(H - HEX) 275, 285, 285
C.....EXPONENTIAL D-E
  275 FND = FCE * FSQ * EXP (ALP * (H - HD))
  285 IF (FI (2, K))345, 345, 290
  290 IF (HB1 - H)295, 295, 345
  295 IF (H - HT1)300, 300, 345
  300 IF (LIN)305, 305, 335
  305 Z = (H - HI (2, K)) / YI (2, K)
C.....PARABOLIC F1
      FN1 = FC1 * (1.0 - Z * Z)
      GO TO 345
C.....LINEAR F1
  335 FN1 = S1*(H - (HI(2,K)-YI(2,K) ) )
  345 IF (HB2 - H)355, 355, 365
  355 Z = (H - HI (3, K)) / YI (3, K)
C.....PARABOLIC F2
      FN2 = FC2 * (1.0 - Z*Z)
  365 FNSQ (IH,1) = AMAX1 (FND, FNE, FNVAL, FN1, FN2)
C.....USE THE MAXIMUM
  375 CONTINUE
      RETURN
      END
C--------------------------------
