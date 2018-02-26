c###outtop.for
      SUBROUTINE OUTTOP
C---------------------------------
C
C     THIS ROUTINE OUTPUTS THE HEADER INFORMATION WHICH INCLUDES THE
C     METHOD, VERSION NUMBER OF THE PROGRAM AND PAGE NUMBER. OTHER
C     OUTPUT CONSISTS OF THE USER DEFINED INPUT INFORMATION AND DEPENDS
C     ON THE METHOD. SUBROUTINE SETOUT SETS THE LINES TO BE OUTPUT IN AN
C     ARRAY CALLED LINTOP(I) WHERE THE INDEX REFERS TO THE FOLLOWING
C     =1 OUTPUT MONTH, YEAR AND SUNSPOT NUMBER & MINIMUM TAKE-OFF ANGLE
C     =2 OUTPUT INFORMATION ON "LABEL" CONTROL CARD
C     =3 OUTPUT TRANSMITTER AND RECEIVER INFORMATION CONSISTING OF
C        COORDINATES, AZIMUTH AND GREAT CIRCLE DISTANCE
C     =4 OUTPUT TRANSMITTER ANTENNA INFORMATION & OUTPUT POWER
C     =5 OUTPUT RECEIVER ANTENNA INFORMATION
C     =6 3 MHZ MAN-MADE NOISE, REQUIRED RELIABILITY AND REQUIRED SNR
C     =7 OUTPUT THE MULTIPATH POWER TOLERANCE AND DELAY TIME TOLERANCE
C
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /ctime/ ntime                             !  plot vs time
      common /cmodel/ model
      character model*8
      common /ccoeff/ coeff
      character coeff*4
      common /cdaily/ idaily(12)
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,20),aeff(30,20)
      character anttype*10,antname*70,antfile*24
      common /pantenna/ pwrkw(20),pwrdba(20)
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      common /CVERSN/ VERSN
         character VERSN*8
      COMMON / OUTPRT / LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      COMMON / SSP / SUN(2,12), MONTH
      common /Cday/ iday
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      CHARACTER ITOP*1
      CHARACTER path*6
      CHARACTER MFORM*1
      DATA MFORM /''/

      if(ndistance.ne.1) then         !  only test if distance plot
         if(idistance.ne.1 .or. ihr.ne.1) return
      end if
      if(ntime.ne.0 .and. ihr.ne.1) return
      JOUT = LUO
      KNT = 0
C.....INCREMENT PAGE NUMBER
      LPAGES= LPAGES + 1
      ITOP='~'
      IF(JTX.NE.1)ITOP=' '
      meth=METHOD
      if(mspec.eq.121) meth=30
      if(iday.eq.0) then
        WRITE(JOUT,1500) MFORM,coeff,ITOP, meth, model,VERSN, LPAGES
 1500   FORMAT(a1,5x,a4,' Coefficients',8x,
     +       A1,'METHOD',I3,1X,a8,' L ',a8,'  PAGE',I4,/)
      else
        WRITE(JOUT,1501) MFORM,'URSI',ITOP, meth, model,VERSN, LPAGES
 1501   FORMAT(a1,5x,a4,' Coeff(Daily)',8x,
     +       A1,'METHOD',I3,1X,a8,' L ',a8,'  PAGE',I4,/)
      end if

      if(LINTOP(1).gt.0 .and. MONTH.gt.0) then
         if(iday.le.0) then
             WRITE(JOUT,1502) IMON(MONTH), NYEAR, SSN,AMIND
 1502        FORMAT(2X,A3,3X,A5,10X,'SSN = ',F4.0,16x,
     +             'Minimum Angle=',f6.3,' degrees')
         else
             WRITE(JOUT,1503) IMON(MONTH), iday,NYEAR, SSN,AMIND
 1503        FORMAT(2X,A3,1h,,i2.2,A5,10X,'SSN = ',F4.0,16x,
     +             'Minimum Angle=',f6.3,' degrees')
         end if
      end if
      path='    '
      if(NPSL.ne.0) path='<Long>'
      if(LINTOP(2).gt.0) WRITE(JOUT,1504)  ITRAN,IRCVR,path
 1504 FORMAT(2X,4A10,2X,'AZIMUTHS',2X,a6,2x,'N. MI.',6X,'KM')
      if(LINTOP(3).gt.0) then
        GCDNMI = GCDKM*0.54
        ITLAT='N'
        ITLONG='E'
        IRLAT='N'
        IRLONG='E'
        if(TLATD.lt.0.) ITLAT='S'
        if(TLONGD.lt.0.) ITLONG='W'
        if(RLATD.lt.0.) IRLAT='S'
        if(RLONGD.lt.0.) IRLONG='W'
        WRITE(JOUT,1506) abs(TLATD),ITLAT,abs(TLONGD),ITLONG,
     +                   abs(RLATD),IRLAT,abs(RLONGD),IRLONG,
     +                   BTRD,BRTD,GCDNMI,GCDKM
      end if
c*****************************************************************
      if(LINTOP(4).gt.0) then
         do 50 i=1,numants               !  TRANSMITTER (with power)
         if(iats(i).ne.1) go to 50
         write(LUO,1520) 'XMTR',nint(xfqs(i)),nint(xfqe(i)),
     +         anttype(i),antfile(i)(1:21),
     +         beammain(i),offazim(i),pwrkw(i)
1520     format(2x,a4,i3,'-',i2,1x,a10,1h[,a21,'] Az=',f5.1,
     +       ' OFFaz=',f5.1,f8.3,'kW')
         KNT=KNT+1
50       continue
      end if
      if(LINTOP(5).gt.0) then
         do 55 i=1,numants                !  RECEIVER (no power)
         if(iats(i).ne.2) go to 55
         write(LUO,1520) 'RCVR',nint(xfqs(i)),nint(xfqe(i)),
     +         anttype(i),antfile(i)(1:21),beammain(i),offazim(i)
         KNT=KNT+1
55       continue
      end if
c*****************************************************************
  161 if(LINTOP(6).gt.0) WRITE(JOUT,1514) -ZNOISE, LUFP, RSN
      if(LINTOP(7).gt.0) WRITE(JOUT,1516) PMP,DMP
      LINES = LINTOP(15) + KNT
      RETURN
 1506 FORMAT(F7.2,1X,A1,F8.2,1X,A1,' - ',F5.2,1X,A1,F8.2,
     A 1X,A1,F10.2,F8.2,F10.1,F9.1)
 1514 FORMAT(2X,'3 MHz NOISE = ',F6.1,
     A ' dBW     REQ. REL = ',I2,'%    REQ. SNR =',F5.1,' dB')
 1516 FORMAT(2X,'MULTIPATH POWER TOLERANCE = ',F4.1,' dB   ',
     A 'MULTIPATH DELAY TOLERANCE = ',F6.3,' ms')
      END
C--------------------------------
