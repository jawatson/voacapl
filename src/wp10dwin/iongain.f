c# gain.f
      SUBROUTINE ionGAIN(kop,toaz,ysig,yeps,ynd,ynl,ynh,tex,
     +                DELTA,FMC,RAIN,EFF)

      dimension tex(4)
      call       ionGAIN2(kop,toaz,ysig,yeps,ynd,ynl,ynh,tex,
     +                DELTA,FMC,RAIN,EFF)
c          this is to adjust type 3 & 4 antennas so 90 deg elevation
c          angle is consistent no matter what the azimuth
      if(kop.eq.3 .or. kop.eq.4) then
         toaz_0=0.
         delta_90=1.570796
         call    ionGAIN2(kop,toaz_0,ysig,yeps,ynd,ynl,ynh,tex,
     +                DELTA_90,FMC,RAIN_90,EFF_90)
         gmorph=sin(delta)**4
         rain=rain_90*gmorph + rain*(1.-gmorph)
      end if
      return
      end
c--------------------------------------------------------------
      SUBROUTINE ionGAIN2(kop,toaz,ysig,yeps,ynd,ynl,ynh,tex,
     +                DELTA,FMC,RAIN,EFF)
      use error_codes
C---------------------------------
C.....ITR .LT. 0 INDICATES TO CALCULATE GROUND REFLECTION LOSS
C     ITR = 1 INDICATES TRANSMITTER ANTENNA
C     ITR = 2 INDICATES RECEIVER ANTENNA
C
C     POWER GAIN OF ANTENNA AND GROUND REFLECTION LOSS
C     FOR LOSS, KOP IS ZERO OR MINUS
C     DIMENSION A(10),B(10),CA(10),SA(10)
c toaz  is off azimuth in degrees
C DELTA IS ELEVATION ANGLE,RADIANS
C FMC  IS FREQUENCY,MHZ.
C BETA IS 0.0 FOR LOSS AND VARIES WITH ANTENNA TYPE.
C SIGMA IS GROUND CONDUCTIVITY, MHOS/METER.
C ER IS GROUND RELATIVE DIELECTRIC CONSTANT.
C PHI,EL,H AND EX(4) VARY WITH ANTENNA TYPE
C RAIN IS LOSS OR GAIN.
C
C
C     THE REFLECTION COEFFICIENTS ARE
C     KSUBV = -CV * EXP(I * PSIV),  KSUBV = CV * EXP(I * GAMMAV)
C     KSUBH = -CH * EXP(I * PSIH),  KSUBH = CH * EXP(I * GAMMAH)
C     IE  NORMALIZED WITH PSI = GAMMA + PI, PSI = GAMMA - PI
C
C     IONCAP REPORT VOLUME 1 PAGE 115 OR ITSA-1 PAGE 65
C     N O T E THIS IS NOT THE SAME AS MA AND WALTERS, ITS-74, PAGE 8
C
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
ccc      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD
ccc      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),SIGPAT
ccc     A (5),EPSPAT(5)
      DIMENSION CA(10), SA(10)
      DIMENSION EX(4),tex(4)
      save
      DATA RINTW/18.06/
      COT(X) = 1. / TAN(X)
C.....RINTW IS "RA" FOR VERTICAL MONOPOLE AT H/LAMDA = .2
ccc      write(*,'('' in iongain, kop='',i5,10f8.3)')
ccc     +         kop,toaz,ysig,yeps,ynd,ynl,ynh,tex
      SOK = 0.0
      RAIN=0.
      EFF = 0.0
      floor=-30.      !  minimum antenna gain value
c********************************************************
      SIGMA = YSIG
      ER = YEPS
      BETA = TOAZ
      PHI = YND
      EL = YNL
      H = YNH
      DO 535 IK = 1,4
  535 EX(IK) = TEX(IK)
      if (SIGMA.le.0.) then
           write (* ,'('' Error: SIGMA<=0.'')') 
           write (* ,'('' GAIN:'')')      
           call exit(EC_EXEC_ERROR)
      end if
c*********************************************************************
C.....THE EQUATIONS FOR THE FRESNEL REFLECTION COEFFICIENTS ARE IN
C.....VOLUME I OF THE OT REPORT ON THIS ANALYSIS PROGRAM
      SOK = 0.
      if(delta.le.0.) then        !  elevation angle=0  force gain=-30
         rain=floor
         go to 610
      end if
      RELTA = DELTA
c           for vertical beam, force off azimuth=0 so answers will be the same
ccc      if(abs(DELTA-PIO2).lt..0001) BETA=0.
      ibeta=nint(beta)
      X = 18000. * SIGMA / FMC
      T = COS (RELTA)
      Q = SIN (RELTA)
      R = Q * Q
      S = R * R
      ERT = ER - T * T
      RHO = SQRT ((ERT) * (ERT) + X * X)
      RHO12 = SQRT (RHO)
      ALPHA = - ATAN (X / ERT)
      U = (ER * ER + X * X)
      V = SQRT (U)
      ASXV = ASIN (X / V)
      CV = SQRT (RHO * RHO + U * U * S - 2. * RHO * U * R * COS (ALPHA +
     1 2. * ASXV)) / (RHO + U * R + 2. * RHO12 * V * Q * COS (ALPHA * .5
     2 + ASXV))
      RAIN = 0.
      A = 2. * RHO12 * Q * V * SIN (ALPHA * .5 + ASXV)
      WAVE = 299.7925 / FMC
      B = RHO - U * R
      IF (B)135, 115, 140
 115  IF (A)120, 125, 130
 120  PSIV = - 1.570796
      GO TO 145
 125  PSIV = 0.
      GO TO 145
 130  PSIV = 1.570796
      GO TO 145
 135  PSIV = ATAN (A / B) + 3.141593
      GO TO 145
 140  PSIV = ATAN (A / B)
  145 SOK = EX(1)
      RETA = BETA * .017453293
      PSIH = ATAN (2. * RHO12 * Q * SIN (ALPHA * .5) / (RHO - R))
      SB = SIN (RETA)
      CB = COS (RETA)
      X = 1.
      CH = SQRT (RHO * RHO + S - 2. * RHO * R * COS (ALPHA)) / (RHO + R
     1+ 2. * RHO12 * Q * COS (ALPHA * .5))
      if(abs(cv-ch).le..001) cv=ch
      IF (EL)165, 185, 185
 165  EL1 = ABS(EL)
      FAC = 3.1416 * EL1
      GO TO 190
 185  FAC = 3.1416 * EL / WAVE
      EL1 = EL / WAVE
 190  IF (H)200, 200, 195
 195  HWAVE = 6.2832 * H / WAVE
      H1 = H / WAVE
      GO TO 220
  200 H1 = ABS(H)
      HWAVE = 6.2832 * H1
  220 RHI = PHI * .017453293
C.....THE EQUATIONS USED FOR EACH OF THE ANTENNAS SPECIFIED BELOW ARE IN
C.....VOLUME I OF THE OT REPORT ON THIS ANALYSIS PROGRAM
      GO TO(230,350,340,340,345,270,265,245,265,240), KOP
C
C    TERMINATED RHOMBIC                          KOP=1
C
 230  U1 = 1. - T * SIN (RHI + RETA)
      U2 = 1. - T * SIN (RHI - RETA)
      RAIN = 3.2 * (COS(RHI) * SIN(FAC * U1) * SIN(FAC * U2) / (U1 *
     1U2)) * * 2 * ((CB - SIN (RHI) * T) * * 2 * (CH * CH + 1. - 2. * CH
     2 * COS (PSIH - 2. * HWAVE * Q)) + SB * SB * (CV * CV + 1. - 2. * C
     3V * COS (PSIV - 2. * HWAVE * Q)) * R)
      GO TO 385
C
C      TERMINATED INTERLACED RHOMBIC             KOP=10
C
 240  HT = EX (1)
      SOK = 0.
      SS = EX (2)
      E = SIN (RHI)
      F = COS (RHI)
      D = SQRT (SS * SS + HT * HT)
      SG = HT / D
      CG = SS / D
      HTWAVE = 6.283185308 * HT / WAVE
      YH = PSIH - 2. * HWAVE * Q
      YV = PSIV - 2. * HWAVE * Q
      U1 = 1. - T * SIN (RETA + RHI)
      U2 = 1. - T * SIN (RHI - RETA)
      ELFAC = F * SIN (FAC * U1) * SIN (FAC * U2) / (U1 * U2)
      ELFAC2 = ELFAC * ELFAC
      Y = 6.283185308 / WAVE * (SS - D * (Q * SG + T * CG * CB))
      Z = Y - 2. * HTWAVE * Q
      H1 =1.+COS(Y) - CH * (COS(YH) * (1. + COS(Z)) - SIN(YH) * SIN(Z))
      H2 = - SIN(Y) - CH * (-SIN(YH) * (1. + COS(Z)) - COS(YH) * SIN(Z))
      HK = H1 * H1 + H2 * H2
      BRK = CB - E * T
      HRAIN = ELFAC2 * BRK * BRK * HK
      V1 = 1.+COS(Y) - CV*(COS(YV) * (1. + COS(Z)) - SIN(YV) * SIN(Z))
      V2 = -SIN(Y) - CV * (-SIN(YV) *(1. + COS(Z)) - COS(YV) * SIN(Z))
      VK = V1 * V1 + V2 * V2
      BRV = SB * SB * R
      VRAIN = ELFAC2 * BRV * VK
      RAIN = (HRAIN + VRAIN) * 0.8
      GO TO 385
C
C      INVERTED L                                KOP=8
C
 245  FAC2 = FAC * 2.
      FAC4 = FAC * 4.
      TWAVE = HWAVE * .15916
      IF(TWAVE - .20) 250, 251, 251
  250 EFF = TWAVE * (6.335 + TWAVE*(67.95 - TWAVE*(693. - 1600.*TWAVE)))
      EFF = 20.*ALOG10(EFF)
      GO TO 252
  251 EFF = 0.0
  252 HAC2 = HWAVE + HWAVE
      HAC4 = HAC2 + HAC2
      CALL CISI(HAC2,W5,W6)
      CALL CISI(HAC4,W7,W8)
      CIN2 = .577215 + ALOG(HAC2) - W5
      CIN4 = .577215 + ALOG(HAC4) - W7
      RA = 30. * (-.5 * COS(HAC2) * CIN4 + (1. + COS(HAC2)) * CIN2 +
     1 SIN(HAC2) * (.5 * W8 - W6))
      U = T * SB
      HK = 1. + CH * CH - 2. * CH * COS (PSIH - 2. * HWAVE * Q)
      C = 1. - COS(FAC2*U) * COS(FAC2) - U * SIN(FAC2*U) - 0.5 *
     1 SIN(FAC2) * SIN(FAC2) * (1. - U * U)
      HQ = Q * HWAVE
      CHQ = COS(HQ)
      SHQ = SIN(HQ)
      CFAC2 = COS(FAC2)
      SFAC2 = SIN(FAC2)
      A = CFAC2 * CHQ - Q * SFAC2 * SHQ - COS(HWAVE + FAC2)
      B = Q * SFAC2 * CHQ + CFAC2 * SHQ - Q * SIN(HWAVE + FAC2)
      BPRIM = 0.0
      if(A.ne.0.) BPRIM = ATAN(B / A)
      SPHIP = 1. - U * U
      G2 = 0.0
      if(SPHIP.ne.0.) G2 = HK * (2. * C * CB / SPHIP) ** 2
      F1 = 0.0
      if(T.ne.0.)F1=(A*A+B*B)*(1.-2.*CV*COS(PSIV- 2.*BPRIM)+CV*CV)/(T*T)
      RAIN = 30. * (G2 + F1) / RA
      X = 1.
      GO TO 385
C
C.....TERMINATED SLOPING VEE           KOP = 7,
C.....AND TERMINATED SLOPING RHOMBIC   KOP = 9.
  265 HT = EX(1)
      SOK = 0.
      IF(HT.LT.0.)THEN
        HT = ABS(HT)
      ELSE
        HT=HT/WAVE
      ENDIF
      G = (HT - H1) / EL1
      IF(KOP .EQ. 9) G = 0.5 * G
      P = SQRT(1. - G * G)
      E = SIN (RHI)
      F = COS (RHI)
      RHI = ASIN (E / P)
      SISUM = SIN (RHI + RETA)
      SIDIF = SIN (RETA - RHI)
      COSUM = COS (RHI + RETA)
      CODIF = COS (RHI - RETA)
      COPSI1 = Q * G + T * P * CODIF
      COPSI2 = Q * G + T * P * COSUM
      COPSI3 = - Q * G + T * P * CODIF
      COPSI4 = - Q * G + T * P * COSUM
      COPSI5 = T * G + Q * P * CODIF
      COPSI6 = T * G + Q * P * COSUM
      COPSI7 = - T * G + Q * P * CODIF
      COPSI8 = - T * G + Q * P * COSUM
      U1 = 1. - COPSI1
      U2 = 1. - COPSI2
      U3 = 1. - COPSI3
      U4 = 1. - COPSI4
      W1 = COS(PSIH - 2. * HWAVE * Q)
      W2 = SIN(PSIH - 2. * HWAVE * Q)
      W3 = COS(PSIV - 2. * HWAVE * Q)
      W4 = SIN(PSIV - 2. * HWAVE * Q)
      FAC2 = FAC * 2.
      IF(KOP - 9) 268, 235, 235
  268 Y1 = U1 * SISUM
      Y2 = U2 * SIDIF
      Y3 = U3 * SISUM
      Y4 = U4 * SIDIF
      Z1 = COS (FAC2 * U1) - 1.
      Z2 = COS (FAC2 * U2) - 1.
      Z3 = COS (FAC2 * U3) - 1.
      Z4 = COS (FAC2 * U4) - 1.
      V1 = SIN (FAC2 * U1)
      V2 = SIN (FAC2 * U2)
      V3 = SIN (FAC2 * U3)
      V4 = SIN (FAC2 * U4)
      UC27 = U2 * COPSI7
      UC18 = U1 * COPSI8
      UC45 = U4 * COPSI5
      UC36 = U3 * COPSI6
      RAIN = .025 * (P * P * (((Y2 * Z1 - Y1 * Z2) / (U1 * U2) - CH / (U
     13 * U4) * (Y4 * (W1 * Z3 + W2 * V3) - Y3 * (W1 * Z4 + W2 * V4))) *
     2 * 2 + (( - Y2 * V1 + Y1 * V2) / (U1 * U2) - CH / (U3 * U4) * (Y4
     3* (W2 * Z3 - W1 * V3) - Y3 * (W2 * Z4 - W1 * V4))) * * 2) + ((Z1 *
     4 UC27 - Z2 * UC18) / (U1 * U2) - CV / (U3 * U4) * (W3 * (Z3 * UC45
     5 - Z4 * UC36) + W4 * (V3 * UC45 - V4 * UC36))) * * 2 + (( - V1 * U
     6C27 + V2 * UC18) / (U1 * U2) - CV / (U3 * U4) * (W3 * ( - V3 * UC4
     75 + V4 * UC36) + W4 * (Z3 * UC45 - Z4 * UC36))) * * 2)
      RAIN = 2. * RAIN
      X = 1.
      GO TO 385
C  FINISH TERMINATED SLOPING RHOMBIC , KOP = 9
  235 A6 = 1. + COS(FAC2*(U1+U2)) - COS(FAC2*U1) - COS(FAC2*U2)
      B6 = - SIN(FAC2*(U1+U2)) + SIN(FAC2*U1) + SIN(FAC2 *U2)
      A7 = 1. + COS(FAC2*(U3+U4)) - COS(FAC2*U3) -  COS(FAC2*U4)
      B7 = - SIN(FAC2*(U3+U4)) + SIN(FAC2*U3) + SIN(FAC2*U4)
      X7 = COPSI8/U2 - COPSI7/U1
      Y7 = COPSI5/U3 - COPSI6/U4
      X8 = SIDIF /U1 - SISUM /U2
      Y8 = SIDIF /U3 - SISUM /U4
C.....SWITCH TO MA"S USE
      PSIV = PSIV - PI
      PSIH = PSIH + PI
      W1 = COS(PSIH - 2. * HWAVE * Q)
      W2 = SIN(PSIH - 2. * HWAVE * Q)
      W3 = COS(PSIV - 2. * HWAVE * Q)
      W4 = SIN(PSIV - 2. * HWAVE * Q)
      F7 = X7*A6  + Y7* CV * (A7* W3 - B7 * W4)
      G7 =  X7*B6  + Y7* CV * (A7* W4 + B7 * W3)
      F8 =  X8*A6  + Y8* CH * (A7* W1 - B7 * W2)
      G8 =  X8*B6  + Y8* CH * (A7* W2 + B7 * W1)
      RAIN = .05 *( F7*F7 + G7*G7 + P*P * (F8*F8 + G8*G8 ) )
      GO TO 385
C
C     CURTAIN                                    KOP=6
  270 CONTINUE
      IF(abs(DELTA-PIO2).gt.0001) go to 272    !  not vertical
  271 RAIN = floor
      GO TO 610
  272 CONTINUE
      SOK = 0.
C.....ALLOW SPACING FOR CURTAIN TO BE IN WAVELENGTHS
C.....(TO ALLOW CCIR ANTENNA PATTERNS HR M/N/H WITH EX(4)=-.25)
C
      DY = EX(2) / WAVE                  !  horz spacing bet dipole centers
      IF(EX(2).le.0.) DY=ABS(EX(2))      !  wavelengths
      DZ = EX(3) / WAVE                  !  vert spacing between dipoles
      IF(EX(3).le.0.) DZ=ABS(EX(3))      !  wavelengths
      DX = EX(4) / WAVE                  !  distance to screen
      IF(EX(4).le.0.) DX=ABS(EX(4))      !  wavelengths
C.....HEIGHT OF BOTTOM ELEMENT IS H1
      CHI = CH
      PSIHI = PSIH
      CH = - CHI * COS (PSIHI)
      PSIH = - CHI * SIN (PSIHI)
      IF (AMOD (PHI, 2.))275, 280, 275
 275  EP = 1.
      NEP = 1
      GO TO 285
 280  EP = 0.
      NEP = 0
 285  NN = (PHI - EP) / 2.
      M = EX (1)
      CVI = CV
      PSIVI = PSIV
      CV = - CVI * COS (PSIVI)
      PSIV = - CVI * SIN (PSIVI)
      FAC1 = FAC
      FAC2 = 2. * FAC
      FAC3 = PI * DY
      DENOM = 1. - SB * SB * T * T
      SEL = COS (FAC1 * SB * T) - COS (FAC1)
      SV = SB * Q
      SZ = SIN(2. * PI * DX * CB * T)
      SX = EP
      FAC4 = T * SB
      DO 290 I = 1, NN
      FN = 2 * I - 1 + NEP
  290 SX = SX + 2.0 * COS(FN * FAC3 * FAC4)
      FAC6 = (SEL * SX * SZ) / DENOM
      HREAL = 0.
      HIMG = 0.
      VREAL = 0.
      VIMG = 0.
      EH = 0.
      EV = 0.
      DO 295 J = 1, M
      FM = J - 1
      ARG = 2.0*PI * Q * (H1 + FM * DZ)
      CA(J) = COS(ARG)
 295  SA(J) = SIN(ARG)
      DO 315 J = 1, M
      HREAL = HREAL + 1.0 * (CA(J) * (1. + CH) + PSIH * SA(J))
      HIMG  = HIMG  + 1.0 * (SA(J) * (1. - CH) + PSIH * CA(J))
 315  CONTINUE
      EH = FAC6 * CB * SQRT (HREAL * HREAL + HIMG * HIMG)
      DO 330 J = 1, M
      VREAL = VREAL + 1.0 * (CA(J) * (1. + CV) + PSIV * SA(J))
      VIMG  = VIMG  + 1.0 * (SA(J) * (1. - CV) + PSIV * CA(J))
 330  CONTINUE
      EV = FAC6 * SV * SQRT (VREAL * VREAL + VIMG * VIMG)
      VALUE = SQRT (EH * EH + EV * EV)
      IF(VALUE.le..00001) go to 271
      RAIN = 20.*ALOG10(VALUE)
      SOK = 0.0
      GO TO 610
C
C     HORIZONAL DIPOLE AND HORIZONAL YAGI        KOP=3 AND 4
  340 DUMMY = FAC * 4.
      CALL CISI(DUMMY,W,W1)
      DUMMY = FAC * 2.
      CALL CISI(DUMMY,W2,W3)
      G = COT (FAC)
      SS = SB * SB
      C = T * T
cxxx      RAIN = 4. / ((1. - G * G) * (.5772 + ALOG (4. * FAC) + W) + 4. * G
cxxx     1 * G * (.5772 + ALOG (2. * FAC) + W2) + 2. * G * (W1 - 2. * W3)) *
cxxx     2 ((COS (FAC * SB * T) - COS (FAC)) / (1. - SS * C)) * * 2 * (CB *
cxxx     3CB * (CH * CH + 1. - 2. * CH * COS (PSIH - 2. * HWAVE * Q)) + SB *
cxxx     4 SB * R * (CV * CV + 1. - 2. * CV * COS (PSIV - 2. * HWAVE * Q)))
      ci_kl=w2
      si_kl=w3
      ci_2kl=w
      si_2kl=w1
      cin_kl=.577 + alog(2.*FAC) - ci_kl
      cin_2kl=.577 + alog(4.*FAC) - ci_2kl
      sin2kl=sin(2.*FAC)**2
      sin2kl=1.
      xR=30.*((1.-G*G)*cin_2kl + 4.*G*G*cin_kl +
     +      2.*G*(si_2kl-2.*si_kl))/sin2kl
      xKv=CV*CV + 1. -2.*CV*COS(PSIV-2.*HWAVE*Q)
      xKh=CH*CH + 1. -2.*CH*COS(PSIH-2.*HWAVE*Q)
      xt1=COS(FAC*SB*T) - COS(FAC)
      xb1=1.-SS*C
      xt2=(xKv*SS*R + xKh*CB*CB)/sin2kl
      RAIN=120./xR*(xt1/xb1)**2*xt2
ccc      if(abs(DELTA-PIO2).lt..0001) then      !  90 degrees elevation angle
ccc      if(ibeta.eq.0 .or. ibeta.eq.90) then
ccc         write(15,'('' delta,beta='',2f10.4)') delta,beta
ccc         write(15,'('' sin_delta,cos_delta='',2f10.4)') q,t
ccc         write(15,'('' sin_beta ,cos_beta ='',2f10.4)') SB,CB
ccc         write(15,'('' SS,R,CB='',3e15.7)') SS,R,CB
ccc         write(15,'('' PXIV,PSIH='',2e15.7)') PSIV,PSIH
ccc         write(15,'('' xKv,xKh='',2e15.7)') xKv,xKh
ccc         write(15,'('' xR='',e15.7)') xR
ccc         write(15,'('' xt1='',e15.7)') xt1
ccc         write(15,'('' xb1='',e15.7)') xb1
ccc         write(15,'('' xt2='',e15.7)') xt2
ccc         write(15,'('' RAIN='',e15.7)') RAIN
ccc         write(15,'('' CH,CV='',2e15.7)') CH,CV
ccc      end if
ccc      end if
      GO TO 385
C
C        VERTICAL LOG PERIODIC                   KOP=5
 345  SOK = EX (1)
      GO TO 351
C      VERTICAL MONOPOLE                          KOP = 2
  350 SOK = H
  351 TEMP = H
      H = EL
      EL = TEMP
      E = ER
      IF (H)360, 360, 355
 355  A = 6.283185 * H / WAVE
      X = H / WAVE
      GO TO 380
  360 H1 = ABS(H)
      A = H1 * 6.283185
      X = H1
 380  D = 2. * A
      Z = 2. * D
      CALL CISI (Z, W, W1)
      CALL CISI (D, W2, W3)
      HWAVE = 6.283 * X
      RA = 30. * ( - .5 * COS (D) * (.577215 + ALOG (Z) - W) + (1. + COS
     1 (D)) * (.577215 + ALOG (D) - W2) + SIN (D) * (.5 * W1 - W3))
      IF(EL1 - 0.2) 353, 354, 354
  353 RA = 400. * EL1 * EL1 * RINTW / 16.
  354 CONTINUE
      DENOM = COS(HWAVE*Q) - COS(HWAVE)
      if(DENOM.eq.0.) DENOM = 0.00000001
      BPRIM = ATAN((SIN(HWAVE*Q)-Q*SIN(HWAVE) )/DENOM)
      RAIN = 30. * (((COS (HWAVE * Q) - COS (HWAVE)) / (T * COS (BPRIM))
     1) * * 2 * (CV * CV + 1. - 2. * CV * COS (PSIV - 2. * BPRIM))) / RA
  385 RAIN = AMAX1(RAIN,0.00001)
      RAIN = 10.*ALOG10(RAIN)
C.....DO ANTENNA EFFICIENCY AS ITSA-1 RELBIL.
  610 GO TO (620,625,615,615,625,615,620,630,620,620,615), KOP
  615 EFF = 0.0
      GO TO 405
  620 EFF = -1.7
      GO TO 405
  625 IF(X.ge..35) go to 615
      EFF = (((6416.702573 * X - 6091.33295) * X + 2179.890548) * X - 36
     14.8173803) * X + 25.64620146
      EFF = -EFF
      GO TO 405
C   EFF CALCULATED ABOVE, KOP=8
  630 CONTINUE
  405 RAIN = RAIN + EFF + SOK
      IF(KOP.eq.2) go to 420
      IF(RAIN.le.floor) RAIN=floor + EFF
 420  RETURN
      END
C--------------------------------
