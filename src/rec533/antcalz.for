      SUBROUTINE ANTCALZ(itr,FMC,BANTT,BTRY,DELTD,RAIN)
c          itr=1=TRANSMITTER
c             =2=RECEIVER
c**************************************************************
c          re-written 10/22/92 by Greg Hand to access all
c          antennas as external tables so they can come from any
c          source and simplify addition of new antennas.
c
c          You will notice that antenna gain is now a function
c          of FREQUENCY (FMC). The azimuths are now ignored
c          as the calculated tables have already been computed
c          for the off-azimuths.
c**************************************************************
C DELTD IS ELEVATION ANGLE,DEGREES
C FMC  IS FREQUENCY,MHZ.
C RAIN IS LOSS OR GAIN.
C
      common /cantenna/ numants,iats( 5),anttype( 6),antname( 6),
     +                  xfqs( 5),xfqe( 5),designfreq( 5),antfile( 5),
     +                  beammain( 5),offazim( 5),cond( 5),diel( 5),
     +                  array(30,91,12),aeff(30, 5)
      character anttype*10,antname*70,antfile*24
      integer*2 iarray360(360,91,2)
      dimension freq_calc(10)
      equivalence (array,iarray360)        !  for area coverage
      equivalence (freq_calc,aeff)         !  for REC533 CIRAF test points
c********************************************************
      RAIN=0.
ccc      EFF = 0.0
      if(offazim(itr).lt.-1000.) go to 100   !  special CIRAF test points
      fmax=fmc
      if(fmax.gt.30.) fmax=30.
      do 10 iat=1,numants
c          check to see if transmitter or receiver
      if(iats(iat).ne.itr) go to 10
c          is this in frequency range?
      if(xfqs(iat).le.Fmax .and. Fmax.le.xfqe(iat)) go to 20
10    continue
c          no match found, return GAIN=0
ccc      write(16,11) itr,fmc,bantt,btry,deltd,rain,eff
ccc11    format(' In ANTCALZ =',i3,6f8.3)
      return
c********************************************************
20    if(offazim(iat).eq.-999.) go to 50     !  area coverage antenna
c          Interpolate gain from array
      I = Fmax
      IP1 = I + 1
      IP1 = MIN0(30,IP1)
      FLOATI = I
      XFMC = Fmax - FLOATI
      J = DELTD + 1.
      J= MIN0(J,91)
      if(j.le.0) j=1     !  I don't know why negative elevation angle??
      JP1 = J + 1
      JP1 = MIN0(91,JP1)
      FLOATJ = J
      XDELTA = DELTD - FLOATJ + 1.
      XX = ARRAY(I,J,iat)
      YX = ARRAY(IP1,J,iat)
      XY = ARRAY(I,JP1,iat)
      YY = ARRAY(IP1,JP1,iat)
      RX = XX + XFMC * (YX - XX)
      RY = XY + XFMC * (YY - XY)
      RAIN = RX + XDELTA * (RY - RX)
ccc      EFF=aeff(i,iat) + xfmc*(aeff(ip1,iat)-aeff(i,iat))
ccc      RAIN=RAIN + EFF
ccc      write(61,11) itr,fmc,bantt,btry,deltd,rain!,eff
      RETURN
c************************************************************
c          area coverage antenna
c          negative beammain is for NON-terminated rhombics only
50    ofaz=btry-abs(beammain(iat))   !  transmitter
      if(ofaz.lt.0.) ofaz=ofaz+360.
      i=ofaz+1.
      if(i.gt.360) i=1
      ip1=i+1
      if(ip1.gt.360) ip1=1
      xazim=ofaz-float(i-1)
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
      return
c************************************************************
c          CIRAF test point special case
100   do 110 iat=1,numants
c          check to see if transmitter or receiver
      if(iats(iat).eq.itr) go to 120
110   continue
c          no match found, return GAIN=0
ccc      write(16,11) itr,fmc,bantt,btry,deltd,rain,eff
ccc11    format(' In ANTCALZ =',i3,6f8.3)
      return
120   do 130 ifreq=1,10
      if(abs(freq_calc(ifreq)-fmc).lt..01) go to 140
130   continue
      return          !  frequency match not found
140   J = DELTD + 1.
      J= MIN0(J,91)
      JP1 = J + 1
      JP1 = MIN0(91,JP1)
      XDELTA = DELTD - FLOAT(J-1)
      RX = ARRAY(ifreq,J,iat)
      RY = ARRAY(ifreq,JP1,iat)
      RAIN = RX + XDELTA*(RY - RX)
      return
c************************************************************
      END
C--------------------------------
