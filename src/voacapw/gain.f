c# gain.f
      SUBROUTINE GAIN(ITR,DELTA,FMC,RAIN,EFF)
c**************************************************************
c          re-written 10/22/92 by Greg Hand to access all
c          antennas as external tables so they can come from any
c          source and simplify addition of new antennas.
c**************************************************************
C--------------------------------
C.....ITR .LT. 0 INDICATES TO CALCULATE GROUND REFLECTION LOSS
C     ITR = 1 INDICATES TRANSMITTER ANTENNA
C     ITR = 2 INDICATES RECEIVER ANTENNA
C
C     POWER GAIN OF ANTENNA AND GROUND REFLECTION LOSS
C     FOR LOSS, KOP IS ZERO OR MINUS
C DELTA IS ELEVATION ANGLE,RADIANS
C FMC  IS FREQUENCY,MHZ.
C SIGMA IS GROUND CONDUCTIVITY, MHOS/METER.
C ER IS GROUND RELATIVE DIELECTRIC CONSTANT.
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
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,20),aeff(30,20)
      character anttype*10,antname*70,antfile*24
      common /Careaant/ iarray360(360,91,2)
         integer*2 iarray360
ccc      equivalence (array,iarray360)        !  for area coverage
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),
     A            SIGPAT(5),EPSPAT(5)
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
c********************************************************
      RAIN=0.
      EFF = 0.0
      IF(ITR.lt.0) go to 75
      do 10 iat=1,numants
c          check to see if transmitter or receiver
      if(iats(iat).ne.itr) go to 10
c          is this in frequency range?
      if(xfqs(iat).le.FMC .and. FMC.le.xfqe(iat)) go to 20
10    continue
c          no match found, return GAIN=0
      return
c********************************************************
20    if(offazim(iat).eq.-999.) go to 50     !  area coverage antenna
c********************************************************
c          Interpolate gain from array
      I = FMC
      IP1 = I + 1
      IP1 = MIN0(30,IP1)
      XFMC = FMC - FLOAT(I)
      DELTD = DELTA * R2D
      J = DELTD + 1.
      J= MIN0(J,91)
      JP1 = J + 1
      JP1 = MIN0(91,JP1)
      XDELTA = DELTD - FLOAT(J-1)
      XX = ARRAY(I,J,iat)
      YX = ARRAY(IP1,J,iat)
      XY = ARRAY(I,JP1,iat)
      YY = ARRAY(IP1,JP1,iat)
      RX = XX + XFMC * (YX - XX)
      RY = XY + XFMC * (YY - XY)
      RAIN = RX + XDELTA * (RY - RX)
      EFF = AEFF(I,iat) +XFMC*(AEFF(IP1,iat)-AEFF(I,iat))
      RETURN
c************************************************************
c          area coverage antenna
50    eff=aeff(1,iat)               !  constant frequency
c          negative beammain is for NON-terminated rhombics
      if(iat.eq.1) ofaz=btrd-abs(beammain(iat))   !  transmitter
      if(iat.eq.2) ofaz=brtd-beammain(iat)   !  receiver
      if(ofaz.lt.0.) ofaz=ofaz+360.
      i=ofaz+1.
      if(i.gt.360) i=1
      ip1=i+1
      if(ip1.gt.360) ip1=1
      xazim=ofaz-float(i-1)
      DELTD = DELTA * R2D
      J = DELTD + 1.
      J= MIN0(J,91)
      JP1 = J + 1
      JP1 = MIN0(91,JP1)
      XDELTA = DELTD - FLOAT(J-1)
      XX = iARRAY360(I,J,iat)
      YX = iARRAY360(IP1,J,iat)
      XY = iARRAY360(I,JP1,iat)
      YY = iARRAY360(IP1,JP1,iat)
      RX = XX + Xazim*(YX - XX)
      RY = XY + Xazim*(YY - XY)
      RAIN = RX + XDELTA*(RY - RX)
      rain=rain/100.
ccc      if(iat.eq.1) then
ccc         write(72,51) beammain(iat),ofaz,btrd,deltd,rain,i,j,
ccc     +              iarray360(i,j,iat)
ccc51       format('main,ofaz,btrd,elev,gain=',5f10.3,2i4,i8)
ccc         if(i.eq.1 .and. j.eq.18) then
ccc            write(72,52) (iarray360(1,jj,1),jj=1,10)
ccc52          format('iarray=',10i7)
ccc         end if
ccc      end if
      return
c************************************************************
c          Ground Reflection LOSS
   75 IG = IABS(ITR)
      SIGMA = SIGPAT(IG)
      ER = EPSPAT(IG)
      IF(SIGMA.le.0) RETURN
C.....THE EQUATIONS FOR THE FRESNEL REFLECTION COEFFICIENTS ARE IN
C.....VOLUME I OF THE OT REPORT ON THIS ANALYSIS PROGRAM
      RELTA = DELTA
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
C
C     GROUND LOSS
C
      CH = SQRT(RHO*RHO + S - 2.*RHO*R*COS(ALPHA))/
     1         (RHO + R + 2.*RHO12*Q*COS(ALPHA*.5))
      RAIN = 4.3429*ALOG(.5*(CH*CH + CV*CV))
      RAIN = ABS(RAIN)
      IF(DELTA.le.0.00000001 ) RAIN=6.
      RETURN
      END
C--------------------------------
